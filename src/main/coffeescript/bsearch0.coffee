describe("binary search", ->
  it("can find index of element in array", ->
    expect(indexOf(1, [])).toEqual(-1)
  )
)

indexOf = ->
  -1

number = 42