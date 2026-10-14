import { expect, mock, test } from 'claude-code/testing'
import type { On, RenderSurface } from 'claude-code'
import type { Engine } from 'claude-code/testing'

// The clock, $.store (what other sessions wrote), codex-chat's line and the
// surfaces the session draws on. Returns the status lines the mod pinned and
// how many times it ran codex-chat.
function host(on: On, store: Map<string, unknown>, codex = '', surfaces: RenderSurface[] = ['desktop']) {
  const clock = mock.clock(on)
  const pinned: (string | undefined)[] = []
  const runs = { codex: 0 }
  on('store.get', async (_$, e) => ({ value: store.get(e.key) }) as never)
  on('store.set', async (_$, e) => (store.set(e.key, e.value), { value: undefined }) as never)
  on('process.run', async () => (runs.codex++, { value: { exitCode: codex ? 0 : 1, stdout: codex, stderr: '' } }) as never)
  on('session.surfaces', async () => ({ value: surfaces }) as never)
  on('ui.status', async (_$, e) => (pinned.push(e.text), { value: undefined }) as never)
  on('session.start', async (_$, e) => ({ cwd: e.cwd }) as never)
  on('session.measure', async (_$, e) => ({ changed: e.changed }) as never)
  on('session.attach', async (_$, e) => ({ clientId: e.clientId }) as never)
  return { clock, pinned, runs }
}

// refresh runs unawaited from session.start and the timer, through several
// host calls; let it finish before asserting.
async function settle() {
  for (let i = 0; i < 500; i++) await Promise.resolve()
}

async function start($: Engine) {
  await $.session.start({ cwd: '/repo', surface: 'desktop', source: 'startup' } as never)
  await settle()
}

async function measure($: Engine, fiveHour: number, week: number) {
  await $.session.measure({
    context: { window: 200_000 },
    rateLimits: [
      { kind: 'five_hour', percentUsed: fiveHour },
      { kind: 'seven_day', percentUsed: week },
    ],
    changed: ['rateLimits'],
  } as never)
  await settle()
}

test('desktop: pins Claude and Codex quota left', async ($, on) => {
  const { pinned } = host(on, new Map(), 'codex 5h 100% wk 81%\n')
  await start($)
  await measure($, 23.4, 40)
  expect(pinned.at(-1)).toBe('claude 5h 77% wk 60% | codex 5h 100% wk 81%')
})

test('desktop: a fresh session shows the reading another session stored', async ($, on) => {
  const { pinned } = host(on, new Map([['claude', { fiveHour: 50, week: 10, at: 1 }]]))
  await start($)
  expect(pinned.at(-1)).toBe('claude 5h 50% wk 10%')
})

test('desktop: a reading of its own is stored for the other sessions', async ($, on) => {
  const store = new Map<string, unknown>()
  host(on, store)
  await start($)
  await measure($, 10, 20)
  expect(store.get('claude')).toMatchObject({ fiveHour: 90, week: 80 })
})

test('desktop: picks up a newer reading another session stores', async ($, on) => {
  const store = new Map<string, unknown>()
  const { clock, pinned } = host(on, store)
  await start($)
  await measure($, 10, 20)
  store.set('claude', { fiveHour: 30, week: 70, at: clock.now() + 1 })
  await clock.advance(60_000)
  await settle()
  expect(pinned.at(-1)).toBe('claude 5h 30% wk 70%')
})

test('desktop: clears the line without any figure', async ($, on) => {
  const { pinned } = host(on, new Map())
  await start($)
  expect(pinned.at(-1)).toBeUndefined()
})

test('desktop: starts polling Codex when the app attaches after the start', async ($, on) => {
  const surfaces: RenderSurface[] = []
  const { pinned } = host(on, new Map(), 'codex 5h 100% wk 81%', surfaces)
  await start($)
  surfaces.push('desktop')
  await $.session.attach({ surface: 'desktop', clientId: 'desktop:default' } as never)
  await settle()
  expect(pinned.at(-1)).toBe('codex 5h 100% wk 81%')
})

test('desktop: starts polling Codex on a reading when it missed the start', async ($, on) => {
  const { pinned, runs } = host(on, new Map(), 'codex 5h 100% wk 81%')
  await measure($, 23.4, 40)
  await measure($, 23.4, 40)
  expect(pinned.at(-1)).toBe('claude 5h 77% wk 60% | codex 5h 100% wk 81%')
  expect(runs.codex).toBe(1)
})

test('terminal: leaves the figures to the status line, but stores its readings', async ($, on) => {
  const store = new Map<string, unknown>()
  const { clock, pinned, runs } = host(on, store, 'codex 5h 100% wk 81%', ['terminal'])
  await start($)
  await measure($, 23.4, 40)
  await clock.advance(60_000)
  await settle()
  expect(pinned).toEqual([])
  expect(runs.codex).toBe(0)
  expect(store.get('claude')).toMatchObject({ fiveHour: 77, week: 60 })
})
