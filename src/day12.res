let data = Node.Fs.readFileSync("./data/day12.txt", #utf8)->String.trim

let isBigCave = str => str->Js.String.toUpperCase == str
let isSmallCave = str => !(str->isBigCave)

let toGraphTuple = str => {
  let contents = str->Js.String2.split("-")
  (contents[0], contents[1])
}

let assembleGraph = arr =>
  arr->Belt.Array.reduce(Js.Dict.empty(), (acc, cur) => {
    let (node, edge) = cur

    let maybeEdges = acc->Js.Dict.get(node)
    switch maybeEdges {
    | Some(edges) => {
        let newEdges = Js.Array.concat(edges, [edge])
        acc->Js.Dict.set(node, newEdges)
      }
    | None => acc->Js.Dict.set(node, [edge])
    }

    acc
  })

let lines = data->Js.String2.split("\n")
let graph = lines->Belt.Array.map(toGraphTuple)->assembleGraph

// Don't visit small caves more than once.
let dfs = graph => {
  let stack = ["start"]
  let visited = Js.Dict.empty()
  let break = ref(false)

  while stack->Array.length > 0 && !break.contents {
    switch stack->Js.Array2.pop {
    | Some("end") => Js.log("all done!")
    | Some(node) =>
      switch visited->Js.Dict.get(node) {
      | Some(_) => ()
      | None => {
          visited->Js.Dict.set(node, true)
          Js.log(node)

          switch graph->Js.Dict.get(node) {
          | Some(edges) =>
            edges->Js.Array2.forEach(edge => {
              switch visited->Js.Dict.get(edge) {
              | Some(_) => ()
              | None => {
                  let _ = stack->Js.Array2.push(edge)
                }
              }
            })
          | None => ()
          }
        }
      }
    | None => ()
    }
  }
}

// Js.log(graph)
graph->dfs
