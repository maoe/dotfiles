// Show Claude and Codex quota left under the prompt, as the status line does in
// the terminal. The desktop app draws no status line, so this pins the same
// line with $.ui.status there, and stays quiet in a terminal session.
//
// Quota is account-wide, but Claude's reaches a session only through its own API
// responses (`session.measure`). Each session therefore writes its latest
// reading to `$.store`, shared by every session, and reads the newest one back
// on a timer, so a fresh session shows a figure before its first response.
import type { EngineInterface, Register, SessionRateLimit } from 'claude-code'

// Percent left in each window, and when the reading came in.
type Quota = { fiveHour: number | null; week: number | null; at: number }

const REFRESH_MS = 60_000

// codex-chat caches its quota for 60 s and prints nothing when its app-server
// is not running.
const CODEX = `
bin=$(command -v codex-chat || ls -d "$HOME"/.claude/plugins/cache/claude-skills/codex-chat/*/bin/codex-chat 2>/dev/null | sort -V | tail -n1)
[ -n "$bin" ] && "$bin" status --line 2>/dev/null
`

function left(limits: SessionRateLimit[], kind: string): number | null {
  const l = limits.find(l => l.kind === kind)
  return l ? Math.round(100 - l.percentUsed) : null
}

function newer(a: Quota | null, b: Quota | null): Quota | null {
  if (!a) return b
  if (!b) return a
  return a.at >= b.at ? a : b
}

// The latest figures, one per load of the module; refresh fills them again.
type Figures = { claude: Quota | null; codex: string | null }

export function format(claude: Quota | null, codex: string | null): string | undefined {
  const parts: string[] = []
  if (claude && (claude.fiveHour !== null || claude.week !== null)) {
    parts.push(`claude 5h ${claude.fiveHour ?? '-'}% wk ${claude.week ?? '-'}%`)
  }
  if (codex) parts.push(codex)
  return parts.length ? parts.join(' | ') : undefined
}

// The terminal's status line already shows these figures.
async function inApp($: EngineInterface): Promise<boolean> {
  const surfaces = await $.session.surfaces()
  return surfaces.length > 0 && !surfaces.includes('terminal')
}

async function show($: EngineInterface, f: Figures) {
  if (await inApp($)) $.ui.status(format(f.claude, f.codex))
}

async function refresh($: EngineInterface, f: Figures) {
  f.claude = newer(f.claude, ((await $.store.get('claude')) as Quota | undefined) ?? null)
  try {
    const { exitCode, stdout } = await $.process.run(['bash', '-c', CODEX], { timeoutMs: 10_000 })
    f.codex = exitCode === 0 && stdout.trim() ? stdout.trim() : null
  } catch {
    // No codex-chat, or it hung: show Claude's figures alone.
    f.codex = null
  }
  await show($, f)
}

// The refresh timer, once started.
type Timer = { cancel?: () => void }

// Nothing is shown in a terminal session, so poll nothing there. The desktop
// app joins the roster after session.start, and a reloaded module sees no
// session.start at all, so every hook that may first find it tries again.
async function poll($: EngineInterface, f: Figures, timer: Timer) {
  if (timer.cancel || !(await inApp($)) || timer.cancel) return
  timer.cancel = $.clock.every(REFRESH_MS, () => void refresh($, f)).cancel
  void refresh($, f)
}

export const register: Register = on => {
  const f: Figures = { claude: null, codex: null }
  const timer: Timer = {}

  on('session.start', async ($, e, next) => {
    const result = await next(e)
    await poll($, f, timer)
    return result
  })

  on('session.attach', async ($, e, next) => {
    const result = await next(e)
    await poll($, f, timer)
    return result
  })

  on('session.measure', async ($, e, next) => {
    // A terminal session's readings still go to $.store, for the desktop
    // sessions to show.
    if (e.changed.includes('rateLimits') && e.rateLimits.length > 0) {
      f.claude = {
        fiveHour: left(e.rateLimits, 'five_hour'),
        week: left(e.rateLimits, 'seven_day'),
        at: await $.clock.now(),
      }
      await $.store.set('claude', f.claude)
      await show($, f)
    }
    await poll($, f, timer)
    return next(e)
  })
}
