describe("binary search", ->
  it("can find index of element in array", ->
    expect(indexOf(1, [])).toEqual(-1)

    expect(indexOf(0, [1])).toEqual(-1)
    expect(indexOf(1, [1])).toEqual(0)
    expect(indexOf(2, [1])).toEqual(-1)

    expect(indexOf(0, [1, 2])).toEqual(-1)
    expect(indexOf(1, [1, 2])).toEqual(0)
    expect(indexOf(2, [1, 2])).toEqual(1)
    expect(indexOf(3, [1, 2])).toEqual(-1)

    expect(indexOf(0, [1, 2, 3])).toEqual(-1)
    expect(indexOf(1, [1, 2, 3])).toEqual(0)
    expect(indexOf(2, [1, 2, 3])).toEqual(1)
    expect(indexOf(3, [1, 2, 3])).toEqual(2)
    expect(indexOf(4, [1, 2, 3])).toEqual(-1)

    expect(indexOf(0, [1, 2, 3, 4])).toEqual(-1)
    expect(indexOf(1, [1, 2, 3, 4])).toEqual(0)
    expect(indexOf(2, [1, 2, 3, 4])).toEqual(1)
    expect(indexOf(3, [1, 2, 3, 4])).toEqual(2)
    expect(indexOf(4, [1, 2, 3, 4])).toEqual(3)
    expect(indexOf(5, [1, 2, 3, 4])).toEqual(-1)
  )
)

indexOf = (value, array, shift = 0) ->
  if array.length == 0 then return -1

  midIndex = Math.floor(array.length / 2)
  midValue = array[midIndex]

  if value == midValue then midIndex + shift
  else if value < midValue then indexOf(value, array[0...midIndex], shift)
  else indexOf(value, array[(midIndex + 1)..], shift + midIndex + 1)