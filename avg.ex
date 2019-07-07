defmodule Avg do

  def avg(a, b) do
    (a + b) / 2
  end

  def avg(a, b, c) do
    (a + b + c) / 3
  end

  def avg([]) do
    raise ArgumentError, message: "empty list not allowed!"
  end

  def avg(l) do
    (Enum.sum l) / (length l)
  end
end
