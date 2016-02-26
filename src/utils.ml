module String = struct
  let length = String.length
  let make = String.make
  let concat = String.concat
  let repeat s n =
    let buf = Buffer.create (n * (String.length s)) in
    for i = 1 to n do Buffer.add_string buf s done;
    Buffer.contents buf
  let rchop ?(n = 1) s =
    let slen = String.length s in
    if slen <= n then "" else String.sub s 0 (slen - n)
end

module List = struct
  let at = List.nth
  let hd = List.hd
  let length = List.length
  let init size f =
    if size = 0 then []
    else
      let rec loop cur = function
        | 0 -> [f 0] @ cur
        | ii -> loop ([f ii] @ cur) (ii - 1) in
      loop [] (size - 1)
  let map = List.map
  let map2 = List.map2
  let max = function
    | [] -> assert false
    | hd::tl -> List.fold_left Pervasives.max hd tl
  let interleave ?first ?last (sep:'a) (l:'a list) =
    let may_prepend maybe_x lst = match maybe_x with
      | None -> lst
      | Some x -> x :: lst
    in
    let rec loop acc = function
      | [] -> acc
      | x :: xs ->
        match acc with
        | [] -> loop [x] xs
        | _ -> loop (x :: sep :: acc) xs
    in
    let res = loop [] l in
    may_prepend first (List.rev (may_prepend last res))
end
