let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.unsafe_to_string s)


let list_fill filler ammount =
    let rec aux amm result =
        if amm == 0 then result
        else aux (amm -1) (filler::result)
    in aux ammount []


let current_label = ref 0;;

let next_label = 
    fun () ->
      current_label := (!current_label) + 1;
      "LBL_" ^ (string_of_int (!current_label));;
    

