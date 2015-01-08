Compilation : commande "make" dans ce dossier.

Exécution : ./main.byte <options> <nom(s) de fichier>

Options :
(Les deux premières, présentes dans le code fourni, n'ont pas été modifiées.)
	-print : N'affiche que le résultat du scheduling
	-n <entier> : Nombre de cycles à simuler
	-rand : Les entrées sont générées aléatoirement au lieu d'être démandées en ligne de commande. Permet d'effectuer facilement des tests avec un grand nombre de cycles.
	-wait : Attend à la fin de chaque cycle que l'utilisateur appuie sur entrée. Laisse le temps détudier ce qu'il se passe lorsqu'il n'y a pas d'entrée ou que -rand est utilisé.
	

-------


Dossiers :
	src_unmodified contient les fichiers source fournis qui n'ont pas été modifiés.
	src contient graph.ml, scheduler.ml et les fichiers source que j'ai ajoutés.
	test_unmodified contient les tests fournis.
	test contient des tests que j'ai ajoutés.


-------


Choix techniques et commentaires :


- Une simulation :
	* Initialisation des registres à 0, des RAM à 0 dans toutes les cellules, et des ROM en demandant à l'utilisateur (voir paragraphe "ROM")
	* Cycles
- Un cycle :
	* Lecture des entrées
	* Traitement des équations
	* Phase d'écriture (concerne les registres et les RAM ; voir paragraphe "Phase d'écriture")
	* Impression des sorties
	

- Au besoin, on considère une nappe de 1 fil comme un fil et vice-versa. En effet, dans le test ram.net, on a une équation _l_14_44 = _l_11_60 où _l_11_60 est un fil et _l_14_44 est une nappe de taille 1. On pourrait a priori restreindre ce polymorphisme à quelques opérations (dont CONCAT), mais pour simplifier on a choisi de toujours l'utiliser.

- Registres : on gère les registres grâce à une table de hachage globale dont le fonctionnement est précisé dans le paragraphe "Phase d'écriture". Cela permet de dire dans le scheculer que pour si "b = REG a", b ne dépend de personne. En particulier, on n'a pas de problème avec d'éventuels cycles de registres.

- Mémoires : il y a autant de mémoires distinctes que d'équations associées à une ROM ou une RAM ; elles sont représentées par des tableaux de 2^(addr_size) cellules contenant des tableaux de word_size booléens, et rangées dans une table de hachage (rom_tbl ou ram_tbl) avec comme clé l'ident de l'équation.

- ROM : on initialize les ROM avant le premier cycle, en demandant en ligne de commande, successivement, le contenu de chaque cellule de chaque ROM.
On considère qu'il y a un cycle si l'argument d'une ROM ('read_addr') dépend directement de sa sortie (sans registres sur le chemin bien sûr).

- RAM : comme elles sont modifiables, on initialize simplement les RAM à 0 (de la bonne taille). On considère qu'il y a un cycle si 'read_addr' dépend de la sortie de la RAM, mais pas si les autres entrées en dépendent. Par exemple, dans le test fourni ram.net, l'entrée 'data' (variable "_l_16") dépend de la sortie. C'est pourquoi, dans le scheduler, on considère que l'équation concernant la RAM dépend seulement de 'read_addr'. En effet, lorsqu'on traite cette équation, on n'effectue que la lecture de la RAM. L'écriture est faite en fin de cycle (voir "Phase d'écriture"). Ce n'est pas très satisfaisant de complètement dissocier lecture et écriture ainsi, mais je ne voyais pas comment faire autrement sans considérer qu'il y a un cycle dans ram.net : pour écrire, j'ai besoin d'avoir calculé 'data' (variable "_l_16"), mais pour calculer la variable "_l_16", j'ai besoin d'avoir lu dans la RAM pour calculer la variable "o"...

- Phase d'écriture : concerne les registres et les RAM. Au cours de la phase de traitement des équations, pour les RAM on ne fait que la lecture, et pour un registre "b = REG a" on se contente d'attribuer à b la valeur associée à la clé b dans une table de hachage reg_tbl. Lorsqu'on a fini de traiter toutes les équations, on effectue une phase d'écriture où pour chaque RAM, on effectue l'écriture (on sait que les valeurs de 'write_addr', 'write_enable' et 'data' ont bien été calculées pour ce cycle), et pour chaque registre "b = REG a", on associe à la clé b dans reg_tbl la valeur actuelle de a.