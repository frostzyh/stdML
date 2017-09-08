fun careful_player2(cl,goal) =
  let fun research_discard(held, new_card, total_value) =
	case held of
	    [] => NONE
	  | card::xs' => if total_value - card_value(card) + card_value(new_card) = goal
			 then SOME([Discard(card),Draw])
			 else research_discard(xs', new_card, total_value)

      fun helper(cl,held,moves) =
	case (cl, score(held,goal) = 0) of
	    (_,true) => moves
	  | ([],false) => moves
	  | (card::xs',false) => if sum_cards(held) <= goal - 11
				 then helper(xs',held@[card],moves@[Draw])
				 else case research_discard(held, card, sum_cards(held)) of
					  NONE => moves
					| SOME(move) => helper(xs',held@[card],moves@move)
  in helper(cl,[],[])
  end;
