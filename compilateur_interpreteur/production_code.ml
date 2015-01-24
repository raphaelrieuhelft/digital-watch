let rec convertit n lim = match lim with
    | 0 -> if n > 0 then failwith "Dépassement taille, n est trop grand"
            else []
    | lim -> (n mod 2)::(convertit (n/2) (lim -1))



