open OcamlPcre

type align =
  | Left
  | Right
  | Center

let ansiR = {|\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]|}
let getLength s = Utils.String.length (Pcre.replace ~pat:ansiR s)

let pad ~align ~totalWidth content =
  let freeSpace = totalWidth - (getLength content) in

  match align with
  | Left -> content ^ (Utils.String.make freeSpace ' ')
  | Right -> (Utils.String.make freeSpace ' ') ^ content
  | Center ->
    let leftSpace = freeSpace / 2 in
    (* if freeSpace is odd, give rightSpace one more space *)
    let rightSpace = freeSpace - leftSpace in
    (Utils.String.make leftSpace ' ') ^ content ^ (Utils.String.make rightSpace ' ')

let drawHorizontalLine ~style:(left, mid, right, horizontal) ~row ~maxes =
  if horizontal = "" then ""
  else
    Utils.List.map2 (fun cell maxWidth -> Utils.String.repeat horizontal maxWidth) row maxes
    |> Utils.List.interleave ~first:left ~last:(right ^ "\n") mid
    |> Utils.String.concat ""

let column l i = Utils.List.map (fun row -> getLength @@ Utils.List.at row i) l

type style = {
  top: string * string * string * string;
  middle: string * string * string * string;
  bottom: string * string * string * string;
  vertical: string * string * string;
}

let simple = {
  top = ("┌", "┬", "┐", "─");
  middle = ("├", "┼", "┤", "─");
  bottom = ("└", "┴", "┘", "─");
  vertical = ("│", "│", "│");
}
let double = {
  top = ("╔", "╦", "╗", "═");
  middle = ("╠", "╬", "╣", "═");
  bottom = ("╚", "╩", "╝", "═");
  vertical = ("║", "║", "║");
}
let dotted = {
  top = (" ", " ", " ", "┄");
  middle = (" ", " ", " ", "┄");
  bottom = (" ", " ", " ", "┄");
  vertical = ("┆", "┆", "┆");
}
let onlyVertical = {
  top = (" ", " ", " ", "");
  middle = ("│", "│", "│", "");
  bottom = (" ", " ", " ", "");
  vertical = ("│", "│", "│");
}
let onlyHorizontal = {
  top = (" ", "─", " ", "─");
  middle = (" ", "─", " ", "─");
  bottom = (" ", "─", " ", "─");
  vertical = (" ", " ", " ");
}
let compact = {
  top = ("", "", "", "");
  middle = ("", "", "", "");
  bottom = ("", "", "", "");
  vertical = ("", "", "");
}
let onlyInner = {
  top = (" ", " ", " ", " ");
  middle = (" ", "┼", " ", "─");
  bottom = (" ", " ", " ", " ");
  vertical = (" ", "│", " ");
}
let onlyOuter = {
  top = ("┌", "─", "┐", "─");
  middle = ("", "", "", "");
  bottom = ("└", "─", "┘", "─");
  vertical = ("│", " ", "│");
}
let testCode = {
  top = (" ", " ", " ", " ");
  middle = ("", "", "", "");
  bottom = (" ", " ", " ", " ");
  vertical = ("", "│", "");
}

let table ?(align=Left) ?(style=simple) ?(padding=1) lists =
  let {
    top = (tLeft, tMid, tRight, tBar);
    middle = (mLeft, mMid, mRight, mBar);
    bottom = (bLeft, bMid, bRight, bBar);
    vertical = (vLeft, vMid, vRight);
  } = style in
  let anyRow = Utils.List.hd lists in
  (* max columns width *)
  let maxes = Utils.List.init
    (Utils.List.length anyRow)
    (fun i -> padding * 2 + Utils.List.max (column lists i))
  in
  let paddingStr = Utils.String.make padding ' ' in
  (Utils.List.map (fun row ->
    Utils.List.map2 (fun cell maxWidth ->
      (* padding is added on top of totalWidth *)
      pad ~align ~totalWidth:maxWidth (paddingStr ^ cell ^ paddingStr)
    ) row maxes
    |> Utils.List.interleave ~first:vLeft ~last:(vRight ^ "\n") vMid
    |> Utils.String.concat ""
  ) lists)
  |> Utils.List.interleave
    ~first: (drawHorizontalLine ~style:(tLeft, tMid, tRight, tBar) ~row:anyRow ~maxes)
    ~last: (drawHorizontalLine ~style:(bLeft, bMid, bRight, bBar) ~row: anyRow ~maxes)
    (drawHorizontalLine ~style:(mLeft, mMid, mRight, mBar) ~row: anyRow ~maxes)
  |> Utils.String.concat ""
  |> Utils.String.rchop
