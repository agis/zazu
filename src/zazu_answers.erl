-module(zazu_answers).
-export([unknown/0, mentioned/0]).

unknown() ->
  Answers = {
    "δε καταλαβαίνω",
    "EKA δεν είστε;",
    "αυτά τα ονόματα που ζητάτε δεν είναι εδώ",
    "uh?",
    "δηλαδή;",
    "τί εννοείς;",
    "ΣΜΣ"
  },
  element(random:uniform(tuple_size(Answers)) , Answers).

mentioned() ->
  Answers = {
    "ποιος;",
    "ρίχτο",
    "με φώναξε κανείς;",
    "τί έγινε;",
    "παρακαλώ;",
    "παρών",
    "σας ακούω.."
  },
  element(random:uniform(tuple_size(Answers)) , Answers).
