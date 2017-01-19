require './reader_csv'
require './analytics'

events = read_events('../data/rspec/methodevents.csv')
def is_not_deleted(event)
  event != :deleted
end
def within_time_range(event)
  end_date = Time.parse("2010-01-01")
  start_date = (end_date.to_date - 90).to_time
  event.date > start_date and event.date < end_date
end

methods_by_changes = method_events(events).select {|e| is_not_deleted(e) and within_time_range(e) }.
                      group_by(&:method_name).
                      #group_by(&:class_name).
                      #group_by(&:file_name).
                      map { |method_name, method_events| [method_name, method_events.size]}.
                      sort_by{|_, events_count| events_count}
puts methods_by_changes



