from common import expect_to_be_equal

events = []


def log(event):
    events.append(event)


def create_generator():
    log("generator started")
    yield 1
    log("generator finished")


log("main started")

generator = create_generator()
try:
    next(generator)
    next(generator)
except StopIteration:
    log("main StopIteration")

log("main finished")

expect_to_be_equal(events, [
    'main started',
    'generator started',
    'generator finished',
    'main StopIteration',
    'main finished'
])
