(* @authors Sruthi Suresh (sls463), Advika Ravi Kumar (ar732), 
   Aaradhyaa Gyawali (ag2223), Ishaan Nanal (isn6) *)
open Card

module Player = struct
  type hand = Card.card list
  type score = int

  let extract (hand_list : Card.card list list) num : Card.card list =
    List.nth hand_list num

  let hit hand deck used =
    let card = Card.pick_rand_card deck used in
    used := card :: !used;
    print_endline ("The card pulled is: " ^ Card.string_of_card card);
    card :: hand

  let bust (sum : int) = sum > 21
end
