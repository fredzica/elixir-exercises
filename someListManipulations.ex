defmodule SomeListManipulations do

  def only_positive([]), do: []
  def only_positive([x | xs]) when x > 0, do: [x | only_positive(xs)]
  def only_positive([_ | xs]), do: only_positive(xs)

  def maximum([]), do: raise ArgumentError, message: "empty list not allowed!"
  def maximum([x]), do: x
  def maximum([x | xs]) do
    x2 = maximum(xs)
    if x2 >= x, do: x2, else: x
  end

  def reverse([]), do: []
  def reverse([x]), do: [x]
  def reverse([x | xs]), do: reverse(xs) ++ [x]
end
