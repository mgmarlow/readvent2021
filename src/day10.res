open Belt

exception Bad_expression(string)

let data = Node.Fs.readFileSync("./data/day10.txt", #utf8)->String.trim

let peek = arr => arr->Array.getUnsafe(Array.length(arr) - 1)

let consume = (arr: array<string>, expected: string, ~origin) => {
  let last = peek(arr)

  if last == expected {
    arr->Array.slice(~offset=0, ~len=Array.length(arr) - 1)
  } else {
    raise(Bad_expression(origin))
  }
}

let totalPoints = ref(0)
let tallyPoints = c => {
  let nextPoints = switch c {
  | ")" => 3
  | "]" => 57
  | "}" => 1197
  | ">" => 25137
  | _ => 0
  }

  totalPoints.contents + nextPoints
}

let rec buildStack = (~stack=[], line) => {
  if List.length(line) == 0 {
    stack
  } else {
    let (head, tail) = switch line {
    | list{head, ...tail} => (head, tail)
    | list{} => ("", list{})
    }

    let newStack = switch head {
    | ")" => stack->consume("(", ~origin=")")
    | "]" => stack->consume("[", ~origin="]")
    | "}" => stack->consume("{", ~origin="}")
    | ">" => stack->consume("<", ~origin=">")
    | c => Array.concat(stack, [c])
    }

    buildStack(~stack=newStack, tail)
  }
}

let isValid = (line: list<string>) => {
  try {
    let _ = buildStack(~stack=[], line)
    true
  } catch {
  | Bad_expression(origin) => {
      totalPoints := tallyPoints(origin)
      false
    }
  | _ => false
  }
}

// let complete = line => {
//   let stack = buildStack(line)
//   let elements = []

//   while Array.length(stack) > 0 {
//     let last = stack->Js.Array2.pop
//     switch last {
//     | ")" => 
//     | "]" =>
//     | "}" =>
//     | ">" =>
//     }
//   }
// }

let lines =
  data
  ->Js.String2.split("\n")
  ->Array.map(s => s->Js.String2.split("")->List.fromArray)
  ->Array.keep(isValid)

Js.log(totalPoints.contents)
