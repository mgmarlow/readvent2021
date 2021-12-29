open Jest

let startingTemplate = "NNCB"

let rules = Js.Dict.fromList(list{
  ("CH", "B"),
  ("HH", "N"),
  ("CB", "H"),
  ("NH", "C"),
  ("HB", "C"),
  ("HC", "B"),
  ("HN", "C"),
  ("NN", "C"),
  ("BH", "H"),
  ("NC", "B"),
  ("NB", "B"),
  ("BN", "B"),
  ("BB", "N"),
  ("BC", "B"),
  ("CC", "N"),
  ("CN", "C"),
})

describe("day 14", () => {
  open Expect

  test("1 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 1)
    expect(polymer)->toBe("NCNBCHB")
  })

  test("2 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 2)
    expect(polymer)->toBe("NBCCNBBBCBHCB")
  })

  test("3 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 3)
    expect(polymer)->toBe("NBBBCNCCNBBNBNBBCHBHHBCHB")
  })

  test("4 step", () => {
    let polymer = Day14.growPolymer(startingTemplate, rules, 4)
    expect(polymer)->toBe("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
  })
})
