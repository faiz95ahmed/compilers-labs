(* lab1/intsqrt.p *)

begin
  c := 200000000;
  l := 1;
  while (c > 100) and (c mod 100 = 0) do
    c := c div 100;
    l := l * 10;
  end;
  x := (c+1) * l; y := l;
  while (x - y) > 1 do
    m := (x + y) div 2;
    n := m * m;
    print m; print n; newline;
    if n > 200000000 then
      x := m;
      print 0; newline;
    else
      if n < 200000000 then
        y := m;
        print 1; newline;
      end
    end
  end;
  print y; newline
end.