// Part 1 about the 3n+1 conjecture
//=================================


//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long) : Long = {
  if(n == 1) 0
  else if(n % 2 == 0) collatz(n/2)+1
  else collatz(3*n+1)+1
}


//(2)  Complete the collatz_max function below. It should
//     calculate how many steps are needed for each number 
//     from 1 up to a bound and then calculate the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. Again, you should expect bounds in the range of 1
//     up to 1 Million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def get_collatz_tuple(x: Long) : (Long, Long) = {
  (collatz(x), x.toLong)
}

def collatz_max(bnd: Long) : (Long, Long) = {
  (1L to bnd).map(get_collatz_tuple(_)).maxBy(_._1)
}
