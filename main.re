module Bin =
  Set.Make({
    type t = char;
    let compare = Stdlib.compare;
  });

let read_whole_file = filename => {
  let ch = open_in(filename);
  let s = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  s;
};

let corpus = read_whole_file("corpus/lojban.txt") |> String.lowercase_ascii;

let corpus_len = String.length(corpus);

let score = bin => {
  let score = ref(0);
  let last = ref(Bin.mem(corpus.[0], bin));
  for (i in 1 to corpus_len - 1) {
    switch (corpus.[i]) {
    | 'a'..'z' as letter =>
      let uses_bin = Bin.mem(letter, bin);
      if (last^ != uses_bin) {
        incr(score);
      };
      last := uses_bin;
    | _ => ()
    };
  };
  score^;
};

let letters = "abcdefghijklmnopqrstuvwxyz";

let letters_len = String.length(letters);

let iter = f => {
  let iterations = {
    let exp = float_of_int(letters_len);
    let res = 2. ** exp -. 1.;
    int_of_float(res);
  };
  for (n in 0 to iterations) {
    if (n mod 10000 == 0) {
      Printf.eprintf(
        "Progress: %f%%\n%!",
        float_of_int(n) /. float_of_int(iterations) *. 100.,
      );
    };
    let bin = ref(Bin.empty);
    for (i in 0 to letters_len - 1) {
      let letter = letters.[i];
      let x = 1 lsl i;
      if (x land n == x) {
        bin := Bin.add(letter, bin^);
      };
    };
    let size = Bin.elements(bin^) |> List.length;
    if (size == 14) {
      f(bin^);
    };
  };
};

let main = () => {
  let best = ref(0);
  iter(bin => {
    let score = score(bin);
    if (score >= best^) {
      best := score;
      Printf.printf(
        "(score %d): %s\n%!",
        score,
        List.fold_left(
          (acc, x) => acc ++ String.make(1, x),
          "",
          Bin.elements(bin),
        ),
      );
    };
  });
};

main();
