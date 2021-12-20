let data = Node.Fs.readFileSync("./data/day11.txt", #utf8)->String.trim

let matrix =
  data
  ->Js.String2.split("\n")
  ->Js.Array2.map(row => {
    row->Js.String2.split("")->Js.Array2.map(str => str->Js.Float.fromString->Belt.Float.toInt)
  })

let adjacentPoints = (x, y) => {
  [
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    (x + 1, y + 1),
    (x + 1, y - 1),
    (x - 1, y - 1),
    (x - 1, y + 1),
  ]
}

let increaseEnergy = arr => {
  let flashQueue = []

  let newMatrix = arr->Belt.Array.mapWithIndex((i, row) =>
    row->Belt.Array.mapWithIndex((j, v) => {
      switch v {
      | 9 => {
          let _ = flashQueue->Js.Array2.push((i, j))
          0
        }
      | n => n + 1
      }
    })
  )

  (newMatrix, flashQueue)
}

let flash = (arr, i, j) => {
  let flashQueue = []
  let adjacents = adjacentPoints(i, j)

  let newMatrix = arr->Belt.Array.mapWithIndex((i, row) =>
    row->Belt.Array.mapWithIndex((j, v) => {
      //
    })
  )

  (newMatrix, flashQueue)
}

Js.log(matrix)
let result = (steps, flashes) => {
  open Belt

  // let flashes = ref(0)
  let flashQueue = []

  let _ = matrix->Array.mapWithIndex((i, row) =>
    row->Array.mapWithIndex((j, v) => {
      switch v {
      | 9 => {
          // enqueue something here?
          // flashes := flashes.contents + 1
          let _ = flashQueue->Js.Array2.push((i, j))
          0
        }
      | n => n + 1
      }
    })
  )

  // flashes
  0
}
