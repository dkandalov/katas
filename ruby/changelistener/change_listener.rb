require "thread"

class ChangeListener
  def initialize keys, proc
    @state = keys.inject(Hash.new) { |hash, key|
      hash[key] = nil
      hash
    }
    @proc = proc
    @keys = keys
  end

  def update key, value
    raise("Key #{key} is not in excpeted key set #{@keys}") if not @keys.include?(key)

    last_value = @state[key]
    @state[key] = value
    if last_value != value
      @proc.call(key, last_value, value)
    end
  end
end
=begin
cl = ChangeListener.new [1,2,3], proc { |key, last_value, value|
  p key
}
cl.update 1, 2
cl.update 1, 2
cl.update 1, 1
=end

class StateWatcher
  def initialize args
    is_ok = args[:is_ok]
    when_ok = args[:when_ok]
    when_bad = args[:when_bad]

    @queue = Queue.new
    @thread = Thread.new do
      while not Thread.current["should_stop"]
        state = @queue.pop

        new_state = is_ok.call(state)
        if new_state != @last_state
          @last_state = new_state
          if new_state
            when_ok.call(state)
          else
            when_bad.call(state)
          end
        end
      end
    end
    @thread.run
  end

  def update state
    @queue << state
  end

  def stop
    @thread["should_stop"] = true
    @thread.join
  end
end

class StatePusher
  def initialize state_watcher
    Thread.new do
      while not Thread.current["should_stop"]
        sleep(2)
        state_watcher.update("==ooo==")
        state_watcher.update("ooo")
        sleep(2)
        state_watcher.update("===aaaa==")
        state_watcher.update("aaaa")
      end
    end.run
  end
end

watcher = StateWatcher.new(
    :is_ok => proc { |value| value.include?("aaa") },
    :when_ok => proc { |value| p "ok! #{value}" },
    :when_bad => proc { |value| p "bad #{value}" }
)
StatePusher.new(watcher)
sleep(5)
watcher.stop

=begin
q = Queue.new
t1 = Thread.new do
  while true
    sleep(0.1)
    q << "a"
  end
end
t2 = Thread.new do
  while true
      p q.pop
  end
end
t1.join
t2.join
p "end"
=end
