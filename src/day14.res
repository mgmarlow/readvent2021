let data = Node.Fs.readFileSync("./data/day14.txt", #utf8)->String.trim

let readRule = line => {
  open Js.String2

  let el = line->split("->")
  (el[0]->trim, el[1]->trim)
}

let getTemplateInfo = raw => {
  let template = ref("")
  let ruleList = []

  String.split_on_char('\n', raw)->Belt.List.forEachWithIndex((i, line) => {
    if i == 0 {
      template := line
    } else if line != "" {
      let rule = readRule(line)
      ruleList->Js.Array2.push(rule)->ignore
    }
  })

  let rules = Js.Dict.fromArray(ruleList)

  (template.contents, rules)
}

let counts = template => {
  template->Js.String2.split("")->Js.Array2.reduce((acc, cur) => {
    let count = switch acc->Js.Dict.get(cur) {
    | Some(n) => n + 1
    | None => 1
    }

    acc->Js.Dict.set(cur, count)

    acc
  }, Js.Dict.empty())
}

let rec growPolymer = (template, rules, step) => {
  open Js.String2

  if step <= 0 {
    template
  } else {
    let newTemplate = ref("")

    for i in 0 to template->length - 1 {
      let first = template->charAt(i)
      let second = template->charAt(i + 1)
      let pair = first ++ second

      newTemplate := newTemplate.contents ++ first
      switch rules->Js.Dict.get(pair) {
      | Some(c) => newTemplate := newTemplate.contents ++ c
      | _ => ()
      }
    }

    growPolymer(newTemplate.contents, rules, step - 1)
  }
}

let (template, rules) = getTemplateInfo(data)
let t10 = growPolymer(template, rules, 10)
Js.log(t10->counts)

// let t40 = growPolymer(template, rules, 40)
// Js.log(t40->counts)
