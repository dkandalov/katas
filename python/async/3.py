from common import expect_to_be_equal

events = []


def log(event):
    events.append(event)


def subfunction():
    yield 42


def create_generator():
    log("generator started")
    sf = subfunction()
    try:
        yield next(sf)
        next(sf) # no point yielding here because StopIteration is raised by "sf"
    except StopIteration:
        log("generator: StopIteration")
    log("generator finished")


generator = create_generator()
log("main received: " + str(generator.send(None)))
try:
    log(generator.send(None))
except StopIteration:
    log("main: StopIteration")

expect_to_be_equal(events, [
    'generator started',
    'main received: 42',
    'generator: StopIteration',
    'generator finished',
    'main: StopIteration'
])
