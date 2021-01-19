let i = ref 10;;
while !i > 1 do
  decr i;
  print_endline (string_of_int !i)
done