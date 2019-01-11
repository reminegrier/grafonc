# Peut-on me livrer les pizzas que j'ai commandées ?

Guy raffole des pizzas. Souvent, il en commande des quantités astronomiques pour lui et tout ses amis. Il existe plusieurs pizzérias dans la région, réparties sur différentes villes.

Ces villes sont représentées par des sommets dans notre graphe. Entre ces villes, il existe des routes, plus ou moins accidentées. Ces routes sont représentées par les arcs. La capacité des arcs correspond au nombre de pizzas qu'un livreur peut transporter en un seul trajet sur la route (plus la route est accidentée, moins on y transportera de pizzas pour ne pas les faire tomber !).

Problématique : Guy se trouve dans la ville A, et il commande des pizzas dans une pizzéria de la ville B. Il veut savoir si la totalité de sa commande peut lui être livrée en une seule fois ou s'il devra attendre plusieurs livraisons.

Utilisation :

* Compilation : ocamlbuild ftest.byte
* Utilisation : ./ftest.byte GraphesTests/townsAndRoads.txt *villeDepart* *villeArrivee* *nbPizzas* *fichierSortie.txt*

* Exemple : ./ftest.byte GraphesTests/townsAndRoads.txt Arkney Lindow 25 out.txt
  * A la fin de l'éxécution : "La demande du destinataire peut être satisfaite."
  * Guy pourra se faire livrer 25 pizzas de Arkney à Lindow en une seule fois !
