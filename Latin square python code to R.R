# OpenSesame python inline code for creating a 4 x 4 latin square converted to R
# The original python code was written by Sau-Chin Chen
# source of code: https://scchen.com/en/post/2016-09-14-randomization-in-latin-square/

# set subject number here:
  subject_nr = 1
  
  ### Get the subject number
  # original python code: 
  # nr = self.get('subject_nr')
  nr = subject_nr
  
  ### Generate a random sequence based the prior seed
  ### Defind seed by yourself
  # original python code: 
  # import random
  # SEED = 345
  # seq = [0,4,8,12]
  # random.seed(SEED)
  # random.shuffle(seq)
  
  SEED = 345
  seq = c(0,4,8,12)
  set.seed(SEED)
  seq = sample(seq)
  
  ### Shift the subject numbers
  ### Push to the next sequence
  # original python code: 
  # if nr <=16:
  #   nr += seq[0]
  # elif nr > 16 & nr <= 32:
  #   nr += seq[1]
  # elif nr > 32 & nr <= 48:
  #   nr += seq[2]
  # else:
  #   nr += seq[3]
  
  if(nr <= 16){
    nr = nr + seq[1]
  } else if (nr > 16 & nr <= 32){
    nr = nr + seq[2]
  } else if (nr > 32 & nr <= 48){
    nr = nr + seq[3]
  } else{
    nr = nr + seq[4]
  }
  
  ### Assign the list in one Latin Squane
  # if (nr / 4) % 4 == 1:
  #   if nr % 4 == 0:
  #   lst = 'List01.csv'
  # elif nr % 4 == 1:
  #   lst = 'List02.csv'
  # elif nr % 4 == 2:
  #   lst = 'List03.csv'
  # else:
  #   lst = 'List04.csv'
  # elif (nr / 4) % 4 == 2:
  #   if nr % 4 == 1:
  #   lst = 'List01.csv'
  # elif nr % 4 == 2:
  #   lst = 'List02.csv'
  # elif nr % 4 == 3:
  #   lst = 'List03.csv'
  # else:
  #   lst = 'List04.csv'
  # elif (nr / 4) % 4 == 3:
  #   if nr % 4 == 2:
  #   lst = 'List01.csv'
  # elif nr % 4 == 3:
  #   lst = 'List02.csv'
  # elif nr % 4 == 0:
  #   lst = 'List03.csv'
  # else:
  #   lst = 'List04.csv'
  # else:
  #   if nr % 4 == 3:
  #   lst = 'List01.csv'
  # elif nr % 4 == 0:
  #   lst = 'List02.csv'
  # elif nr % 4 == 1:
  #   lst = 'List03.csv'
  # else:
  #   lst = 'List04.csv'
  
  
  if ((nr / 4) %% 4 == 1){
    if (nr %% 4 == 0){
      lst = 'List01.csv'
    } else if (nr %% 4 == 1){
      lst = 'List02.csv'
    } else if (nr %% 4 == 2){
      lst = 'List03.csv'
    } else{
      lst = 'List04.csv'
    }
  } else if ((nr / 4) %% 4 == 2){
    if (nr %% 4 == 1){
      lst = 'List01.csv'
    } else if (nr %% 4 == 2){
      lst = 'List02.csv'
    } else if (nr %% 4 == 3){
      lst = 'List03.csv'
    } else{
      lst = 'List04.csv'
    }
  } else if ((nr / 4) %% 4 == 3){
    if (nr %% 4 == 2){
      lst = 'List01.csv'
    } else if (nr %% 4 == 3){
      lst = 'List02.csv'
    } else if (nr %% 4 == 0){
      lst = 'List03.csv'
    } else{
      lst = 'List04.csv'
    }
  } else{
    if (nr %% 4 == 3){
      lst = 'List01.csv'
    } else if (nr %% 4 == 0){
      lst = 'List02.csv'
    } else if (nr %% 4 == 1){
      lst = 'List03.csv'
    } else{
      lst = 'List04.csv'
    }
  }


### Output [List] to the loop object
# exp.set('List', lst)

lst



### calculating all possible combinations of the letters A B C D if the letters cannot be repeated.

x = LETTERS[1:4]
subset(expand.grid(rep(list(x),4)), unique(c(Var1, Var2, Var3, Var4)) == 4)

grid_pre = expand.grid(rep(list(x),4))

grid = grid_pre[apply(grid_pre, 1, function(x) length(unique(x))) == 4,]

# there are 24 possible combinations
nrow(grid)

