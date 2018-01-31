import asyncio

from common import expect_to_be_equal

events = []


def log(event):
    events.append(event)


def f():
    # The sleep(0) works essentially as "yield" by passing control back to the event loop,
    # without it a() and b() will be executed sequentially.
    return asyncio.sleep(0, result=42)


async def a():
    log("a 1")
    await f()
    log("a 2")
    await f()
    log("a 3")


async def b():
    log("b 1")
    await f()
    log("b 2")
    await f()
    log("b 3")


async def main():
    await asyncio.gather(a(), b())


log("main started")
loop = asyncio.get_event_loop()
loop.run_until_complete(main())
loop.close()
log("main finished")

expect_to_be_equal(events, [
    'main started',
    'b 1',
    'a 1',
    'b 2',
    'a 2',
    'b 3',
    'a 3',
    'main finished'
])
