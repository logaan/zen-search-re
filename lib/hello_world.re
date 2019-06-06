type file = string;
type field = string;
type step = File | Field(file) | Value(file, field);
type output = Silent(step) | Loud(step, string);

let prompt = (step) =>
  switch(step) {
  | File => "Which file would you like to search?"
  | Field(_) => "Which field should we look in?"
  | Value(_, _) => "What's the value you'd like?"
  };

let tick = (step, input) =>
  switch(step) {
  | File => Silent(Field(input))
  | Field(file) => Silent(Value(file, input))
  | Value(file, field) =>
    Loud(File, "You're searching for: " ++ file ++ field ++ input)
  };

let rec repl = (step) => {
  print_endline(prompt(step));
  let input = read_line();
  if (input != "exit") {
    let next_step = switch(tick(step, input)) {
      | Silent(step) => step
      | Loud(step, output) => print_endline(output); step
    }
    repl(next_step)
  }
}

let run = () => {
  print_endline("Hello");
  repl(File);
  print_endline("Goodbye")
}
