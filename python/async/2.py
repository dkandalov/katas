from common import expect_to_be_equal

events = []


def log(event):
    events.append(event)


def create_generator():
    log("generator started")
    n = yield 1
    log("generator received: " + str(n))
    log("generator finished")


log("main started")
try:
    generator = create_generator()
    n = generator.send(None)  # can't send non-None value to a just-started generator
    log("main received: " + str(n))
    generator.send(2)
except StopIteration:
    log("main StopIteration")

log("main finished")

expect_to_be_equal(events, [
    'main started',
    'generator started',
    'main received: 1',
    'generator received: 2',
    'generator finished',
    'main StopIteration',
    'main finished'
])
