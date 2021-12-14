open Belt

exception Bad_expression

let data = Node.Fs.readFileSync("./data/day10.txt", #utf8)
  -> String.trim

let peek = arr => arr->Js.Array2.unsafe_get(Array.length(arr) - 1)

let consume = (arr: array<string>, expected: string) => {
  let last = peek(arr)

  if last == expected {
    arr->Array.slice(~offset=0, ~len=Array.length(arr) - 1)
  } else {
    raise(Bad_expression)
  }
}

let totalPoints = ref(0)
let tallyPoints = c => {
  let nextPoints = switch c {
  | Some(")") => 3
  | Some("]") => 57
  | Some("}") => 1197
  | Some(">") => 25137
  | _ => 0
  }

  totalPoints.contents + nextPoints
}

let rec isValid = (~expressions=[], line: array<string>) => {
  if Array.length(line) == 0 {
    true
  } else {
    // bad mutation.
    let cur = line->Js.Array2.shift
    try {
      let nextExpressions = switch cur {
      | Some(")") => expressions->consume("(")
      | Some("]") => expressions->consume("[")
      | Some("}") => expressions->consume("{")
      | Some(">") => expressions->consume("<")
      | Some(c) => Array.concat(expressions, [c])
      | None => expressions
      }

      isValid(~expressions=nextExpressions, line)
    } catch {
      | Bad_expression => {
        totalPoints := tallyPoints(cur)
        false
      }
      | _ => false
    }
  }
}

let lines = data
  ->Js.String2.split("\n")
  ->Array.map(s => s->Js.String2.split(""))
  ->Array.keep(isValid)

Js.log(lines)
Js.log(totalPoints.contents)
