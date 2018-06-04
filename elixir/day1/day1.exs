defmodule Rectangle do
  def area({w, h}), do: h * w

  def perimeter({w, h}) do
    2 * (h + w)
  end
end

defmodule Square do
  def area({w}), do: Rectangle.area({w, w})

  def area({w, h}) when w == h do
    Rectangle.area({w, w})
  end

  def perimeter({w}) do
    Rectangle.perimeter({w, w})
  end

  def perimeter({w, h}) when w == h do
    Rectangle.perimeter({w, w})
  end
end

r = {3, 4}
IO.puts "The area of rectangle #{inspect r} is #{Rectangle.area r}"
s = {4}
IO.puts "The area of square #{inspect s} is #{Square.area s}"
# IO.puts "The area of rectangle #{inspect r} is #{Square.area r}"


### Maps
book = %{title: "Programming Elixir", author: %{first: "David", last: "Thomas"}}
put_in book.author.first, "Dave"
IO.inspect book
%{author: %{last: "Thomas"}, title: title} = book
IO.puts title


### Lists
defmodule ListExample do
  def print([]), do: :ok
  def print([head|tail]) do
    IO.puts head
    print tail
  end
end
list = [:storm, :sabretooth, :mystique]
ListExample.print(list)
Enum.each list, &(IO.puts &1)

IO.inspect Enum.filter [1, 2, 3], &(&1 > 1)
IO.inspect Enum.reduce [1, 2, 3], &(&1 + &2)
IO.inspect Enum.any? [1, 2, 3], &(&1 > 2)
IO.inspect Enum.all? [1, 2, 3], &(&1 > 2)
IO.inspect Enum.zip [1, 2, 3], [4, 5, 6]


### For comprehensions
defmodule QuickSort do
  def sort([]), do: []
  def sort([head|tail]) do
    sort( for(x <- tail, x <= head, do: x) ) ++
    [head] ++
    sort( for(x <- tail, x > head, do: x) )
  end
end
IO.inspect QuickSort.sort([5, 6, 3, 2, 7, 8])


### Default values
defmodule Secret do
  def hanger(x \\ 18), do: x
end
IO.inspect Secret.hanger()


### Calling Erlang
IO.inspect :string.trim("  A  ", :leading)


### Easy tasks
defmodule EasyTasks do
  _point2d = {10, 42}
  _line = {{0, 0}, {10, 10}}
  _circle = {{0, 0}, 42}
  _polygon = {{0, 0}, {10, 10}, {42, 42}}
  _triangle = {{0, 0}, {10, 10}, {42, 42}}

  def hypotenuse(l1, l2) do
    :math.sqrt(l1*l1 + l2*l2)
  end
end
IO.inspect EasyTasks.hypotenuse(3, 4)

IO.inspect String.to_atom("abc")
IO.inspect Kernel.is_atom(:abc)


### Medium tasks
defmodule MediumTasks do
  def size([]), do: 0
  def size([_|tail]), do: size(tail) + 1

  def max([head]), do: head
  def max([head|tail]) do
    max = max(tail)
    if head > max do
      head
    else
      max
    end
  end

  def word_count(list, result \\ %{})
  def word_count([], result), do: result
  def word_count([head|tail], result) do
    value = Map.get(result, head, 0)
    word_count(tail, Map.put(result, head, value + 1))
  end
end
IO.puts "size"
IO.inspect MediumTasks.size([])
IO.inspect MediumTasks.size([1])
IO.inspect MediumTasks.size([1,2,3])

IO.puts "max"
IO.inspect MediumTasks.max([1])
IO.inspect MediumTasks.max([3,1,2])

IO.puts "word count"
IO.inspect MediumTasks.word_count([])
IO.inspect MediumTasks.word_count([:a])
IO.inspect MediumTasks.word_count([:a, :b, :a, :c, :b, :a])
