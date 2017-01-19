describe("binary search", ->
  it("can find index of element in array", ->
    expect(indexOf(1, [])).toEqual(-1)

    expect(indexOf(i, [1]) for i in [0..2]).toEqual([-1, 0, -1])
    expect(indexOf(i, [1, 2]) for i in [0..3]).toEqual([-1, 0, 1, -1])
    expect(indexOf(i, [1, 2, 3]) for i in [0..4]).toEqual([-1, 0, 1, 2, -1])
    expect(indexOf(i, [1, 2, 3, 4]) for i in [0..5]).toEqual([-1, 0, 1, 2, 3, -1])
  )
)

indexOf = (value, array, shift = 0) ->
  if array.length == 0 then return -1

  midIndex = Math.floor(array.length / 2)
  midValue = array[midIndex]

  if value == midValue then midIndex + shift
  else if value < midValue then indexOf(value, array[0...midIndex], shift)
  else indexOf(value, array[(midIndex + 1)..], shift + midIndex + 1)