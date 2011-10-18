class QFind
  def initialize size
    @data = Array.new(size) { |i| i }
  end

  def << input
    pairs = input.map { |s| s.split(/-/).map { |v| v.to_i } }
    pairs.each { |pair| add_connection pair }
  end

  private

  def add_connection pair
    adjust_data_size(pair[0] > pair[1] ? pair[0] : pair[1])

    left_value = @data[pair[0]]
    right_value = @data[pair[1]]


    if left_value == right_value
      p "#{pair.inspect}: already connected"
      return
    end

    @data.map! do |v|
      v == left_value ? right_value : v
    end
    p pair.inspect + ": " + @data.inspect
  end

  def adjust_data_size max_index
    return if max_index < @data.size

    old_size = @data.size
    (max_index - @data.size + 1).times {|i| @data[old_size + i] = old_size + i}
  end
end

QFind.new(4) << %w{ 0-2 1-4 2-5 3-6 0-4 6-0 1-3 }
#QFind.new(0) << %w{ 3-4 4-9 8-0 2-3 5-6 2-9 5-9 7-3 4-8 5-6 0-2 6-1 }
