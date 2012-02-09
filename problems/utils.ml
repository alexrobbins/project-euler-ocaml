class sieve upper_limit =
  object (self)
    val numbers = Array.create (succ upper_limit) 1
    val mutable primes = ( [] : int list )
    method get_primes =
      primes
    initializer
      numbers.(0) <- 0;
      numbers.(1) <- 0;
      for i = 2 to upper_limit do
        if numbers.(i) = 1
        then begin
          primes <- i :: primes;
          let rec mark_multiples current =
            if current <= upper_limit then begin
              numbers.(current) <- 0;
              mark_multiples (current + i)
            end
          in
          mark_multiples (2 * i)
        end
      done;
      primes <- List.rev primes
  end;;
