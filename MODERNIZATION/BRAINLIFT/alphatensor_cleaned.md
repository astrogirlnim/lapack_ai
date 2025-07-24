Nature

|

Vol˜˚˛

|

˜October˝˛˝˝

|

47

Discovering faster matrix multiplication

algorithms with reinforcement learning

Alhussein

Fawzi

1

,2

žR

, Matej

Balog

1

,2

, Aja

Huang

1

,2

, Thomas

Huber t

1

,2

,

Bernardino

Romera-Paredes

1

,2

, Mohammadamin

Barekat ain

1

, Alexander

Novikov

1

,

Francisco

J.

R. Ruiz

1

, Julian

Schrittwie ser

1

, Grzegorz

Swirszcz

1

, David

Silver

1

, Demis

Hassabis

1

& Pushmeet

Kohli

1

Improving the e˜cienc y of algorithms for fundamental computations can have a

widespread impac t, as it can a˚ec t the overall speed of a large amount of computations.

Matrix multiplication is one such primitive task, occurring in many systemsŠfrom

neural net works to scienti˛c computing routines. The automatic discover y of

algorithms using machine learning o˚ers the prospec t of reaching beyond human

intuition and outperforming the current best human-designed algorithms. However,

automating the algorithm discover y procedure is intricate, as the space of possible

algorithms is enormous. Here we repor t a deep reinforcement learning approach

based on AlphaZero

1

for discovering e˜cient and provably correc t algorithms for the

multiplication of arbitrar y matrices. Our agent, AlphaTensor, is trained to play a

single-player game where the objec tive is ˛nding tensor decompositions within a

˛nite fac tor space. AlphaTensor discovered algorithms that outperform the state-

of-the-ar t complexity for many matrix sizes. Par ticularly relevant is the case of 4˝×˝4

matrices in a ˛nite ˛eld, where AlphaTensor ™s algorithm improves on Strassen™s two-

level algorithm for the ˛rst time, to our knowledge, since its discover y 50 years ago

2

.

We fur ther showcase the ˙exibility of AlphaTensor through di˚erent use- cases:

algorithms with state- of-the-ar t complexity for struc tured matrix multiplication and

improved prac tical e˜cienc y by optimizing matrix multiplication for runtime on

speci˛c hardware. Our results highlight AlphaTensor ™s ability to accelerate the

process of algorithmic discover y on a range of problems, and to optimize for di˚erent

criteria.

We focus on the fundamental task of matrix multiplication, and use

deep reinforcement learning (DRL) to search for provably correct and

efficient matrix multiplication algorithms. This algorithm discovery

process is particularly amenable to automation because a rich space of

matrix multiplication algorithms can be formalized as low-rank decom

-

positions of a specific three-dimensional (3D) tensor

2

, called the matrix

multiplication tensor

3

Œ

7

. This space of algorithms contains the stand

-

ard matrix multiplication algorithm and recursive algorithms such as

Strassen™s

2

, as well as the (unknown) asymptotically optimal algorithm.

Although an impor tant body of work aims at charac terizing the com

-

plexity of the asymptotically optimal algorithm

8

Œ

12

, this does not yield

practical algorithms

5

. We focus here on practical matrix multiplication

algorithms, which correspond to explicit low-rank decompositions of

the matrix multiplication tensor. In contrast to two-dimensional matri

-

ces, for which efficient polynomial-time algorithms computing the rank

have existed for over t wo centuries

13

, f inding low-rank decompositions

of 3D tensors (and beyond) is NP-hard

14

and is also hard in practice.

In fact, the search space is so large that even the optimal algorithm

for multiplying two 3˝×˝3 matrices is still unknown. Nevertheless, in a

longstanding research effort, matrix multiplication algorithms have

been discovered by attacking this tensor decomposition problem using

human search

2

,

15

,

16

, continuous optimization

17

Œ

19

and combinatorial

search

20

. These approaches often rely on human-designed heuristics,

which are probably suboptimal. We instead use DRL to learn to recog

-

nize and generalize over patterns in tensors, and use the learned agent

to predic t efficient decompositions.

We formulate the matrix multiplication algorithm discovery pro

-

cedure (that is, the tensor decomposition problem) as a single-player

game, called TensorGame. At each step of TensorGame, the player

selects how to combine different entries of the matrices to multiply.

A score is assigned based on the number of selected operations required

to reach the correct multiplication result. This is a challenging game

with an enormous action space (more than 10

12

actions for most inter

-

esting cases) that is much larger than that of traditional board games

such as chess and Go (hundreds of actions). To solve TensorGame and

find efficient matrix multiplication algorithms, we develop a DRL agent,

AlphaTensor. AlphaTensor is built on AlphaZero

1

,

21

, where a neural net

-

work is trained to guide a planning procedure searching for efficient

matrix multiplication algorithms. Our framework uses a single agent

to decompose matrix multiplication tensors of various sizes, yielding

https://doi.org /10.1038/s41586-02 2-05172-4

Received: 2 October 2021

Accepted: 2 August 2022

Published online: 5 October 202 2

Open acce ss

Check for update s

1

DeepMind, London, UK.

2

These authors contributed equally: Alhussein Fawzi, Matej Balog, Aja Huang, Thomas Huber t and Bernardino Romera-Parede s.

R

e-mail:

afawzi@deepmind.com

48

|

Nature

|

Vol˜˚˛

|

˜ October ˝˛˝˝

transfer of learned decomposition techniques across various tensors.

To address the challenging nature of the game, AlphaTensor uses a

specialized neural network architec ture, exploits symmetries of the

problem and makes use of synthetic training games.

AlphaTensor scales to a substantiallyˆlarger algorithm space than

what is within reach for either human or combinatorial search. In fact,

AlphaTensor discovers from scratch many provably correct matrix

multiplication algorithms that improve over existing algorithms in

terms of number of scalar multiplications. We also adapt the algo

-

rithm discovery procedure to finite fields, and improve over Strassen™s

two-level algorithm for multiplying 4˝×˝4 matrices for the first time, to

our knowledge, since its inception in 1969. AlphaTensor also discovers a

diverse set of algorithmsŠup to thousands for each sizeŠshowing that

the space of matrix multiplication algorithms is richer than previously

thought. We also exploit the diversity of discovered factorizations to

improve state-of-the-art results for large matrix multiplication sizes.

Through different use-cases, we highlight AlphaTensor™s flexibility

and wide applicability: AlphaTensor discovers efficient algorithms

for structured matrix multiplication improving over known results,

and finds efficient matrix multiplication algorithms tailored to spe

-

cific hardware, by optimizing for actual runtime. These algorithms

multiply large matrices faster than human-designed algorithms on

the same hardware.

Algorithms as tensor decomposition

As matrix multiplication (

A

,˝

B

)˝

˜

˝

AB

is bilinear (that is, linear in both

arguments), it can be fully represented by a 3D tensor: see Fig.ˆ

1a

for

how to represent the 2˝×˝2 matrix multiplication operation as a 3D ten

-

sor of size 4˝×˝4˝×˝4, and refs.

3

,

5

,

7

for more details. We write

n

˜

for the

tensor describing

n

˝×˝

n

matrix multiplication. The tensor

˜

n

is fixed

(that is, it is independent of the matrices to be multiplied), has entries

in {0,˝1}, and is of size

n

2

˝×˝

n

2

˝×˝

n

2

. More generally, we use

nm

p

,,

˜

to

describe the rectangular matrix multiplication operation of size

n

˝×˝

m

with

m

˝×˝

p

(note that

=

n

nnn

,,

˜

˜

). By a decomposition of

˜

n

into

R

rank- one terms, we mean

˜

ˇ

=˘

˘,

(1

)

n

r

R

rr

r

=1

()

()

()

uv

w

where˝˘˝denotes the outer (tensor) product, and

u

(

r

)

,

v

(

r

)

and

w

(

r

)

are all

vectors. If a tensor

˜

can be decomposed into

R

rank-one terms, we say

the rank of

˜

is at most

R

, or

R

Ra

nk

()



˜

. This is a natural extension

from the matrix rank, where a matrix is decomposed into

ˇ˘

r

R

rr

=1

()

()

uv

.

A decomposition of

˜

n

into

R

rank- one terms provides an algorithm

for multiplying arbitrar y

n

˝×˝

n

matrices using

R

scalar multiplications

(see Algorithm

1

). We refer to Fig.ˆ

1b,c

for an example algorithm mul

-

tiplying 2˝×˝2 matrices with

R

˝=˝7 (Strassen™s algorithm).

Crucially, Algorithm

1

can be used to multiply block matrices. By using

this algorithmˆrecursively, one can multiply matrices of arbitrary size, with

the rank R controlling the asymptotic complexity of the algorithm. In par

-

ticular, N˝×˝N matrices can be multiplied with asymptotic complexity

˚

N

()

R

lo

g(

)

n

; see ref.

5

for more details.

DRL for algorithm discover y

We cast the problem of finding efficient matrix multiplication algo

-

rithms as a reinforcement learning problem, modelling the environ

-

ment as a single-player game, TensorGame. The game state after step

t

is described by a tensor

˛

t

, which is initially set to the target tensor

we wish to decompose:

˛

˜

=

n

0

. In each step

t

of the game, the player

selects a triplet (

u

(

t

)

,˝

v

(

t

)

,˝

w

(

t

)

), and the tensor

˛

t

is updated by subtract

-

ing the resulting rank- one tensor:

˘˘

tt

tt

t

1

()

()

()

˛

˛

uv

w

. The goal

of the player is to reach the zero tensor

0

=

t

˛

by applying the smallest

number of moves. When the player reaches the zero tensor, the

sequence of selec ted fac tors satisf ies

uv

w

˜

=ˇ

˘˘

nt

R

tt

t

=1

()

()

()

(where

R

denotes the number of moves), which guarantees the correctness of

the resulting matrix multiplication algorithm. To avoid playing unnec

-

essarily long games, we limit the number of steps to a maximum value,

R

limit

.

For every step taken, we provide a reward of 1 to encourage finding

the shortest path to the zero tensor. If the game terminates with a

non-zero tensor (after

R

limit

steps), the agent receives an additional

terminal reward equal to

˜

(

)

R

li

mi

t

˛

, where

˜

()

R

li

mi

t

˛

is an upper bound

on the rank of the terminal tensor. Although this reward optimizes for

rank (andˆhence forˆthe complexity of the resulting algorithm), other

reward schemes can be used to optimize other properties, such as

practical runtime (see ‚Algorithm discoveryˆresults™ ). Besides, as our

aim is to find exact matrix multiplication algorithms, we constrain

{

u

(

t

)

,˝

v

(

t

)

,˝

w

(

t

)

} to have entries in a user-specif ied discrete set of coeff i

-

cients

F

(for example,

F

˝=˝{2,˝1,˝0,˝1,˝2}). Such discretization is com

-

mon practice to avoid issues with the finite precision of floating

points

15

,

18

,

20

.

To play TensorGame, we propose AlphaTensor (Fig.ˆ

2

), an agent based

on AlphaZero

1

, which achieved

tabula rasa

superhuman performance

in the classical board games of Go, chess and shogi, and on its extension

to handle large action spaces Sampled AlphaZero

21

. Similarly to

AlphaZero, AlphaTensor uses a deep neural net work to guide a Monte

c

1

c

2

c

3

c

4

=

a

1

a

2

a

3

a

4

·

b

1

b

2

b

3

b

4

a

U

=

1

0

1

0

1

0

00

00

11

0

1

00

00

11

0

1

0

Œ1

V

=

10

00

0

01

1

10

1

W

=

10

01

1

00

10

10

01

01

00

10

0

a

1

a

2

a

3

a

4

b

1

b

2

b

3

b

4

c

1

c

3

c

2

c

4

bc

Œ1

0

1

0

1

0

01

0

0

0

Œ1

Œ1

0

1

01

0

0

1

0

Œ1

1

0

0

0

0

1

Œ1

m

1

= (

a

1

+

a

4

)(

b

1

+

b

4

)

m

2

= (

a

3

+

a

4

)

b

1

m

3

=

a

1

(

b

2

Œ

b

4

)

m

4

=

a

4

(

b

3

Œ

b

1

)

m

5

= (

a

1

+

a

2

)

b

4

m

6

= (

a

3

Œ

a

1

)(

b

1

+

b

2

)

m

7

= (

a

2

Œ

a

4

)(

b

3

+

b

4

)

c

1

=

m

1

+

m

4

Œ

m

5

+

m

7

c

2

=

m

3

+

m

5

c

3

=

m

2

+

m

4

c

4

=

m

1

Œ

m

2

+

m

3

+

m

6

Fi g. 1 | Matrix multiplication ten sor a nd algorithms.

a

, Tenso r

˜

2

represen ting

the multiplic ation of two 2˝×˝2 matrices. Tenso r en tries equal to 1 a re depicted

in purple , and 0 en tries are se mi-transpa ren t. The tenso r specifies which en tries

f rom the input matrices to re ad, and where to w rite the result. For example ,

a s

c

1

˝=˝

a

1

b

1

˝+˝

a

2

b

3

, tenso r en tries loc ated at (

a

1

,˝

b

1

,˝

c

1

) and (

a

2

,˝

b

3

,˝

c

1

) a re set to 1.

b

, Stra ssen's algo ri thm

2

fo r multiplying 2˝×˝2 matrices using 7 multiplic ations.

c

, Stra ssen's algo ri thm in tenso r facto r represen tation. The stacked facto rs

U

,

V

and

W

(green, purple and yellow, respectively)ˆprov ide a ran k-7

dec omp osition of

2

˜

(equationˆ(

1

)).ˆThe c o rresp ondence between a rithmetic

operations (

b

) andˆfacto rs (

c

) is shown by using the afo remen tioned c olo urs.

Nature

|

Vol˜˚˛

|

˜October˝˛˝˝

|

49

Carlo tree search (MCTS) planning procedure. The network takes as

input a state (that is, a tensor

t

˛

to decompose), and outputs a policy

and a value. The policy provides a distribution over potential actions.

As the set of potential actions (

u

(

t

)

,˝

v

(

t

)

,˝

w

(

t

)

) in each step is enormous,

we rely on sampling actions rather than enumerating them

21

,

22

. The

value provides an estimate of the distribution

z

of returns (cumulative

reward) starting from the current state

t

˛

. With the above reward

scheme, the distribution

z

models the agent™s belief about the rank of

the tensor

˛

t

. To play a game, AlphaTensor starts from the target tensor

(

n

˜

) and uses the MCTS planner at each step to choose the next ac tion.

Finished games are used as feedback to the network to improve the

net work parameters.

Overcoming the challenges posed by TensorGameŠnamely, an enor

-

mous action space, and game states described by large 3D tensors

representing an abstrac t mathematical operationŠrequires multiple

advances. All these components, described briefly below, ˆsubstantially

improve the overall performance over a plain AlphaZero agent (see

Methods and Supplementary Information for details).

Neural network architec ture

We propose a transformer-based

23

architecture that incorporates

induc tive biases for tensor inputs. We first projec t the

S

˝×˝

S

˝×˝

S

input

tensor into three

S

˝×˝

S

grids of feature vectors by using linear layers

applied to theˆthree cyclicˆtranspositions of the tensor. The main part of

the model comprises a sequence of attention operations, each applied

to a set of features belonging to a pair of grids (Extended Data Figs.ˆ3

and 4). This generalizes axial attention

24

to multiple grids, and is both

more eff icient and yields better results than naive self-attention. The

proposed architecture, which disregards the order of rows and columns

in the grids, is inspired by the invariance of the tensor rank to slice

reordering. The final feature representation of the three matrices is

passed both to the policy head (an autoregressive model) and the value

head (a multilayer perceptron).

Synthetic demonstrations

Although tensor decomposition is NP-hard, the inverse task of con

-

structing the tensor from its rank-one factors is elementary. Hence,

we generate a large dataset of tensor-factorization pairs (synthetic

demonstrations)ˆby first sampling factors

uv

w

{

(,

,)

}

rr

r

r

R

()

()

()

=1

at random,

and then constructing the tensor

=ˇ

˘˘

r

R

rr

r

=1

()

()

()

˝

uv

w

. We train the

network on a mixture of supervised loss (that is, to imitate synthetic

demonstrations) and standard reinforcement learning loss (that is,

learning to decompose a target tensor

n

˜

) (Fig.ˆ

2

). This mixed training

strategyŠtraining on the target tensor and random tensorsŠˆsubstan

-

tially outperforms each training strategy separately. This is despite

randomly generated tensors having different properties from the tar

-

get tensors.

Change of basis

n

˜

(Fig.ˆ

1a

) is the tensor representing the matrix multiplication bilinear

operation in the canonical basis. The same bilinear operation can be

expressed in other bases, resulting in other tensors. These different

Algorithm 1

A met a-algorithm parameterized by

=

uv

w

{,

,}

rr

r

()

()

()

r

R

1

for computing

the matrix product

C

˜=˜

A

B

. It is noted that

R

controls the number of

multiplications between input matrix entrie s.

Parameters:

=

uv

w

{,

,}

rr

r

()

()

()

r

R

1

:

length-

n

2

vectors such that

˜

n

rr

r

()

()

()

r

R

1

=ˇ

˘˘

=

uv

w

Input:

A

,˜

B

: matrice s of size

n

˜×˜

n

Output:

C

˜=˜

A

B

(1)

fo r

r

˜=˜1,˜–,˜

R

do

( 2 ) ˚ ˚˚

++

+

˜˜

mu

au

av

bv

b

()

()

r

n

n

11

r

n

rr

n

r

1

()

()

1

()

()

2

2

22

(3)

fo r

i

˜=˜1,˜–,˜

n

2

do

( 4 ) ˚ ˚˚

˜

cw

mw

m

iR

1

ii

R

(1

)(

)

+

return

C

Change of basis

Pre-generated

synthetic

demonstration

s

Played game

s

buffe

r

Playe

d

game

Sample

random state

Neural network

Policy head

Value head

Acting

...

Learning

Updated

mode

l

Network input

Training labels

(

u

,

v

,

w

)

(

u

(1

)

,

v

(1

)

,

w

(1

)

)

(

u

(2

)

,

v

(2

)

,

w

(2

)

)(

u

(3

)

,

v

(3

)

,

w

(3

)

)

Fi g. 2 | Overview of AlphaTen sor.

The neural netwo rk (bo ttom box) takes

a s input a tenso r

˛

t

, and o utputs samples (

u

,˝

v

,˝

w

) f rom a distribution

over p o ten tial next actions to pl ay, and an estimate of the future returns

(for example , of

˛

R

an

k(

)

t

). The netwo rk is trained on two data so urces:

prev iously pl ayed games and syn thetic demonstrations. The updated netwo rk

is sent to the acto rs (top box), where it is used by the MCTS planner to generate

new games.

50

|

Nature

|

Vol ˜˚˛

|

˜October˝˛˝˝

tensors are equivalent: they have the same rank, and decompositions

obtained in a custom basis can be mapped to the canonical basis, hence

obtaining a practical algorithm of the form in Algorithm

1

. We leverage

this observation by sampling a random change of basis at the beginning

of every game, applying it to

˜

n

, and letting AlphaTensor play the game

in that basis (Fig.ˆ

2

). This crucial step injec ts diversity into the games

played by the agent.

Dat a augment ation

From every played game, we can extract additional tensor-factorization

pairs for training the network. Specifically, as factorizations are

order invariant (owing to summation), we build an additional

tensor-factorization training pair by swapping a random action with

the last ac tion from each f inished game.

Algorithm discover y˜results

Discover y of matrix multiplication algorithms

We train a single AlphaTensor agent to find matrix multiplication algo

-

rithms for matrix sizes

n

˝×˝

m

with

m

˝×˝

p

, where

n

,˝

m

,˝

p

˝˝5. At the begin

-

ning of each game, we sample uniformly a triplet (

n

,

m

,

p

) and train

AlphaTensor to decompose theˆ tensor

˜

nm

p

,,

. Although we consider

tensors of fixed size (

˜

nm

p

,,

has size

n

m

˝×˝

m

p

˝×˝

p

n

), the discovered algo

-

rithms can be applied recursively to multiply matrices of arbitrary size.

We use AlphaTensor to find matrix multiplication algorithms over

different arithmeticsŠnamely, modular arithmetic (that is, multiplying

matrices in the quotient ring

2

˜

), and standard arithmetic (that is, mul

-

tiplying matrices in

˚

).

Figureˆ

3

(left) shows the complexity (that is, rank) of the algo

-

rithms discovered by AlphaTensor. AlphaTensor re-discovers the

best algorithms known for multiplying matrices (for example,

Strassen™s

2

and Laderman™s

15

algorithms). More impor tantly, AlphaT

-

ensor improves over the best algorithms known for several matrix

sizes. In particular, AlphaTensor finds an algorithm for multiplying

4˝×˝4 matrices using 47 multiplications in

2

˜

, thereby outperforming

Strassen™s two-level algorithm

2

, which involves 7

2

˝=˝49 multiplica

-

tions. By applying this algorithm recursively, one obtains a practical

matrix multiplication algorithm in

˜

2

with complexity

˚

N

()

2.

778

.

Moreover, AlphaTensor discovers efficientˆalgorithms for multiply

-

ing matrices in standard arithmetic; for example, AlphaTensor finds

a rank-76 decomposition of

4,

5,

5

˜

, improving over the previous

state-of-the-art complexity of 80 multiplications. See Extended

Data Figs.ˆ1 and 2 for examples.

AlphaTensor generates a large database of matrix multiplication

algorithmsŠup to thousands of algorithms for each size. We exploit

this rich space of algorithms by combining them recursively, with the

aim of decomposing larger matrix multiplication tensors. We refer

to refs.

25

,

26

and Appendix H in Supplementar y Information for more

details. Using this approach, we improve over the state-of-the-art

results for more than 70 matrix multiplication tensors (with

n

,˝

m

,˝

p

˝˝12). See Fig.ˆ

3

(right) and Extended Data Tableˆ1 for the results.

A crucial aspect of AlphaTensor is its ability to learn to transfer knowl

-

edge bet ween targets (despite providing no prior knowledge on their

relationship). By training one agent to decompose various tensors,

AlphaTensor shares learned strategies among these, thereby improv

-

ing the overall performance (see Supplementar y Information for

analysis). Finally, it is noted that AlphaTensor scales beyond current

computational approaches for decomposing tensors. For example, to

our knowledge, no previous approach was able to handle

˜

4

, which has

an action space 10

10

times larger than

˜

3

. Our agent goes beyond this

limit, discovering decompositions matching or surpassing

state- of-the-ar t for large tensors such as

5

˜

.

AlphaTensor rank

Modular Standard

Size

(

n

,

m

,

p

)

Best method

known

Best rank

known

(2,

2,

2)

(

Strassen

,

1969

)

2

7

7

7

(3,

3,

3)

(

Laderman

,

1976

)

15

23

23

23

49

47

49

98

96

98

(2

,2

,3

)

11

11

11

(2

,2

,4

)

14

14

14

(2

,2

,5

)

18

18

18

(2

,3

,3

)

15

15

15

(2

,3

,4

)

20

20

20

(2

,3

,5

)

25

25

25

(2

,4

,4

)

26

26

26

(2

,4

,5

)

33

33

33

(2

,5

,5

)

40

40

40

(3

,3

,4

)

(

Smirnov

,

2013

)

18

29

29

29

(3

,3

,5

)

36

36

36

(3

,4

,4

)

38

38

38

(3

,4

,5

)

48

47

47

(3

,5

,5

)

58

58

58

(4

,4

,5

)

64

63

63

(4

,5

,5

)

80

76

76

200

40

06

00

80

01

,000

Best rank know

n

0

5

10

15

20

25

30

Improvement in ran

k

(9, 9, 9)

(9, 9, 11

)

(9, 10, 10

)

(9, 11, 11

)

(10, 10, 10

)

(10, 11, 12

)

(10, 12, 12

)

(11, 11, 11

)

(11, 12, 12

)

(

Strassen

,

1969

)

2

(

Hopcroft and Kerr

,

1971

)

16

(

Hopcroft and Kerr

,

1971

)

16

(

Hopcroft and Kerr

,

1971

)

16

(

Hopcroft and Kerr

,

1971

)

16

(

Hopcroft and Kerr

,

1971

)

16

(

Smirnov

,

2013

)

18

(

Smirnov

,

2013

)

18

(

Smirnov

,

2013

)

18

(

Sedoglavic and Smirnov

,

2021

)

19

(3,

5,

5) + (2,

5,

5)

(2,

2,

2)

^

(2,

2,

2)

(2,

2,

2) + (2,

2,

1)

(2,

2,

2) + (2,

2,

2)

(2,

2,

2) + (2,

2,

3)

(4,

4,

2) + (4,

4,

3)

(2,

5,

5)

^

(2,

1,

1)

(4,

4,

4)

(5,

5,

5)

(

Hopcroft and Kerr

,

1971

)

16

Fi g. 3 | Comparison b etween th e comple xit y of previously k nown matrix

multiplication algorithms and th e one s discovere d by AlphaTen sor.

Lef t:

c olu mn (

n

,˝

m

,˝

p

) refers to the prob lem of multiplying

n

˝×˝

m

with

m

˝×˝

p

matrices.

The c omplexity is mea sured by the nu mber of scal a r multiplic ations (o r

equivalently, the nu mber of terms in the decomp osition of the tenso r). ‚ Best

ran k known™ refers to the best known upper bou nd on the tenso r ran k (befo re

this paper), wherea s ‚AlphaTenso r rank™ rep o rts the rank upper bo u nds

obtained with o ur metho d, in mo dul a r a rithmetic (

˜

2

) and standa rd a ri thmetic.

In all c a ses, AlphaTenso r disc over s algo rithms that match o r improve over

known state of the a rt (improvemen ts a re shown in red ). See Extended D ata

Figs.ˆ1 and 2 fo r examples of algo rithms fo u nd with AlphaTenso r. Righ t: results

(fo r a rithmetic in

˚

) of applying AlphaTenso r-disc overed algo rithms on l a rger

tenso rs. Each red dot represen ts a tenso r size , with a subset of them l abelled.

See Extended D ata Tab l eˆ1 fo r the results in tab le fo rm. State-of-the-a rt res ults

a re obtained f rom the list in ref.

64

.

Nature

|

Vol˜˚˛

|

˜October˝˛˝˝

|

51

Analysing the symmetries of matrix multiplication algorithms

From a mathematical standpoint, the diverse algorithms discovered

by AlphaTensor show that the space is richer than previously known.

For example, whileˆthe only known rank-49 factorization decomposing

˜

˜˜

=˘

42

2

before this paper conforms to the product structure (that

is, it uses the factorization of

˜

2

twice, which we refer to as Strassen-

square

2

), AlphaTensor finds more than 14,000 non-equivalent fac

-

torizations (with standard arithmetic) that depart from this scheme,

and have different properties (such as matrix ranks and sparsityŠsee

Supplementar y Information). By non-equivalent, we mean that it is

not possible to obtain one from another by applying a symmetry trans

-

formation (such as permuting the fac tors). Such proper ties of matrix

multiplication tensors are of great interest, as these tensors represent

fundamental objects in algebraic complexity theory

3

,

5

,

7

. The study of

matrix multiplication symmetries can also provide insight into the

asymptotic complexity of matrix multiplication

5

. By exploring this rich

space of algorithms, we believe that AlphaTensor will be useful for

generating results and guiding mathematical research. See Supple

-

mentary Information for proofs and details on the symmetries of

fac torizations.

B eyond st andard matrix multiplication

Tensors can represent any bilinear operation, such as structured matrix

multiplication, polynomial multiplication or more custom bilinear

operations used in machine learning

27

,

28

. We demonstrate here a

use-case where AlphaTensor finds a state-of-the-art algorithm for

multiplying an

n

x

n

skew-symmetric matrix ˆwith a vectorˆof length

n

.

Figureˆ

4a

shows the obtained decompositions for small instance sizes

n

.

We observe a pattern that we generalize to arbitrary

n

, and prove that

this yields a general algorithm for the skew-symmetric matrix-vector

produc t (Fig.ˆ

4b

). This algorithm, which uses

nn

n

(

1)

(+

2)/2

~

1

2

2

multiplicationsˆ(whereˆ

˚

ˆindicates asymptotic similarity), outperforms

the previously known algorithms using ˆasymptotically

n

2

multiplica

-

tions

29

, and is asymptotically optimal. See Supplementar y Information

for a proof, and for another use-case showing AlphaTensor™s ability

to re-discover the Fourier basis (see also Extended Data Tableˆ2). This

shows that AlphaTensor can be applied to custom bilinear operations,

and yield efficient algorithms leveraging the problem structure.

Rapid t ailored algorithm discover y

We show a use-case where AlphaTensor finds practically efficient matrix

multiplication algorithms, tailored to specific hardware, with zero

prior hardware knowledge. To do so, we modif y the reward of AlphaT

-

ensor: we provide an additional reward at the terminal state (after the

agent found a correc t algorithm) equal to the negative of the runtime

of the algorithm when benchmarked on the target hardware. That is,

we set

r

r˚

b

=

+

tt

t

, where

r

t

is the reward scheme described in ‚DRL for

algorithm discovery™,

b

t

is the benchmarking reward (non-zero only at

the terminal state) and

˚

is a user-specified coefficient. Aside from the

different reward, the exac t same formulation of TensorGame is used.

We train AlphaTensor to search for efficient algorithms to multiply

4˝×˝4 block matrices, and focus on square matrix multiplication of size

8,192 (each block is hence of size 2,048) to define the benchmarking

reward. AlphaTensor searches for the optimal way of combining the

16 square blocks of the input matrices on the considered hardware. We

do not apply the 4˝×˝4 algorithm recursively, to leverage the efficient

implementation of matrix multiplication on moderate-size matrices

(2,048˝×˝2,048 in this case). We study two hardware devicesˆcommonly

used in machine learning and scientific computing: an Nvidia V100

graphics processing unit (GPU) and a Google tensor processing unit

(TPU) v2. The factorization obtained by AlphaTensor is transformed

into JAX

30

code, which is compiled (just in time) before benchmarking.

Figureˆ

5a,b

shows the efficiency of the AlphaTensor-discovered

algorithms on the GPU and the TPU, respectively. AlphaTensor dis

-

covers algorithms that outperform theˆStrassen-square algorithm,

which is a fast algorithm for large square matrices

31

,

32

. Although

the discovered algorithm has the same theoretical complexity as

Strassen-square, it outperforms it in practice, as it is optimized for

the considered hardware. Interestingly, AlphaTensor finds algorithms

n

=

3

Extrapolatio

n

a

(

n

Œ

1

)(

n

+

2

)

2

multiplications.

(2)

(3)

w

ij

=

a

ij

(

b

j

Œ

b

i

)

Computing the ˜rst (

n

Œ

2

)(

n

+

1

)/2 intermediate products

(4)

(5)

q

i

=

b

i

n

j

=1

a

ji

(7)

(8)

(9)

Input

:

n

×

n

skew-symmetric matrix

A

, vector

b

.

Output

: The resulting vector

c

=

Ab

computed in

(1)

for

i

= 1, . . . ,

n

˜ 2

do

for

j

=

i +

1, . . . ,

n

do

(4)

for

i

= 1, . . . ,

n

do

˜

(6)

for

i

= 1, . . . ,

n

˜ 2

do

Computing the ˚nal

n

intermediate products

c

i

=

i

Œ1

j

=1

w

ji

+

˜

n

j

=

i+

1

w

ij

Œ

q

i

˜

c

n

Œ1

=

Œ

n

Œ2

i

=1

˜

w

ij

Œ

n

Œ2

j

=

i

+1

˜

w

jn

+

n

Œ2

j

=1

˜

q

i

n

i

=1,

i

˛

n

Œ1

˜

c

n

=

Œ

n

Œ1

i

=1

˜

w

ij

+

n

j

=

i

+1

˜

q

i

n

Œ1

i

=1

˜

b

WV

U

n

=

4

n

=

5

n

=

6

n

= 10

Fi g. 4 | Algorithm discovery b eyond stan dard matrix multiplication.

a

, Dec omp osi tions fo u nd by AlphaTenso r fo r the tenso rs of size

nn

××

nn

(

1)

2

(with

n

˝=˝3 ,˝4,˝5 ,˝6) represen ting the skew- symmetric matrix-vector multiplic ation.

The red pixels deno te 1 , the blue pixels deno te 1 and the white pixels deno te 0.

Extrap ol ation to

n

˝=˝10 is shown in the righ tmost figure.

b

, S kew- symmetric

matrix-by-vecto r multiplic ation algo rithm, obtained f rom the examples solved

by AlphaTenso r. The

w

i

j

and

q

i

terms in steps 3 and 5 c o rresp ond to the

m

r

terms

in Algo rithm 1. It is noted that steps 6Œ9 do no t involve any multiplic ations.

52

|

Nature

|

Vol ˜˚˛

|

˜October˝˛˝˝

with a larger number of additions compared with Strassen-square (or

equivalently, denser decompositions), but the discovered algorithms

generate individual operations that can be efficiently fused by the

specific XLA

33

grouping procedure and thus are more tailored towards

the compiler stack we use. The algorithms found by AlphaTensor also

provide gains on matrix sizes larger than what they were optimized

for. Finally, Fig.ˆ

5c

shows the importance of tailoring to particular

hardware, as algorithms optimized for one hardware do not perform

as well on other hardware.

Discussion

Trained from scratch, AlphaTensor discovers matrix multiplication

algorithms that are more efficient than existing human and

computer-designed algorithms. Despite improving over known

algorithms, we note that a limitation of AlphaTensor is the need

to pre- def ine a set of potential fac tor entries

F

, which discretizes

the search space but can possibly lead to missing out on efficient

algorithms. An interesting direc tion for future research is to adapt

AlphaTensor to search for

F

. One impor tant strength of AlphaTensor

is its flexibility to support complex stochastic and non-differentiable

rewards (from the tensor rank to prac tical efficiency on specific hard

-

ware), in addition to finding algorithms for custom operations in a

wide variety of spaces (such as f inite f ields). We believe this will spur

applications of AlphaTensor towards designing algorithms that opti

-

mize metrics that we did not consider here, such as numerical stability

or energ y usage.

The discover y of matrix multiplication algorithms has far-reaching

implications, as matrix multiplication sits at the core of many com

-

putational tasks, such as matrix inversion, computing theˆdetermi

-

nant and solving linear systems, to name a few

7

. We also note that our

methodology can be extended to tackle related primitive mathemati

-

cal problems, such as computing other notions of rank (for example,

border rankŠsee Supplementary Information), and NP-hard matrix

fac torization problems (for example, non-negative fac torization). By

tackling a core NP-hard computational problem in mathematics using

DRLŠthe computation of tensor ranksŠAlphaTensor demonstrates the

viability of DRL in addressing difficult mathematical problems, and

potentially assisting mathematicians in discoveries.

Online content

Any methods, additional references, Nature Research reporting summa

-

ries, source data, extended data, supplementary information, acknowl

-

edgements, peer review information; details of author contributions

and competing interests; and statements of data and code availability

are available at

https://doi.org /10.1038/s41 586-022-05 172-4

.

1.

Silver, D. et˚al. A general reinforcement le arning algorithm that masters chess, shogi, and

Go through self-play.

Science

362

, 1140Œ1144 ( 2018).

2.

Strassen, V. Gaussian elimination is not optimal.

Numer. Math.

13

, 354Œ356 (1969).

3.

Bürgisser, P., Clausen, M. & Shokrollahi, A .

Algebraic C omplexity Theor y

Vol. 315 (Springer

Science & Busine ss Media, 2013).

4.

Bläser, M. Fast matrix multiplication.

Theory C omput.

5

, 1Œ60 ( 2013).

5.

Landsberg, J. M.

Geometr y and C omplexity Theory

169 (C ambridge Univ. Pre ss, 2017 ).

6.

Pan, V. Y. Fast fe asible and unfe asible matrix multiplication. Preprint at

https://arxiv.org/

a b s/1 804.041 0 2

( 2018).

7.

Lim, L .-H. Tensors in comput ations.

Acta Numer.

30

, 555Œ764 ( 2021).

8.

Schönhage, A . Par tial and tot al matrix multiplication.

SIAM J. C omput.

10

, 434Œ455 (1981).

9.

Coppersmith, D. & Winograd, S. Matrix multiplication via arithmetic progressions. In

AC M

Symposium on Theor y of C omputing

1Œ6 (ACM, 1987 ).

10.

Strassen, V. The asymptotic spectrum of tensors and the exponent of matrix

multiplication. In

27th Annual Symposium on Foundations of C omputer Science

49Œ54

(IEEE , 1986).

11.

Le Gall, F. Powers of tensors and fast matrix multiplication. In

International Symposium on

Symbolic and Algebraic C omput ation

296Œ303 (ACM, 2014).

12.

Alman, J. & Williams, V. V. A re˛ined laser method and faster matrix multiplication. In

ACM-SIAM Symposium on Discrete Algorithms

522Œ539 (SIAM, 2021).

13.

Gauss, C. F.

Theoria Motus Corporum C oele stium in Sectionibus C onicis Solum

Ambientium

(Per the s and Be sser, 1809).

14.

Hillar, C. J. & Lim, L .-H. Most tensor problems are NP-hard.

J. ACM

60

, 1Œ39 (2013).

15.

Laderman, J. D. A noncommut ative algorithm for multiplying 3˜×˜3 matrice s using 23

multiplications.

Bull. Am. Math. Soc .

82

, 126Œ128 (1976).

16.

Hopcrof t, J. E . & Kerr, L . R. On minimizing the number of multiplications nece ssar y for

matrix multiplication.

SIAM J. Appl. Math.

20

, 30Œ36 (1971).

17.

Ver vliet, N., Debals, O., Sorber, L ., Van Barel, M. & De Lathauwer, L .

Tensorlab 3.0

( 2016);

https://w ww.tensorlab.net/

18.

Smirnov, A . V. The bilinear complexity and practical algorithms for matrix multiplication.

C omput. Math. Math. Phy s .

53

, 1781Œ1795 ( 2013).

19.

Sedoglavic, A . & Smirnov, A. V. The tensor rank of 5x5 matrice s multiplication is bounded

by 98 and its border rank by 89. In

Proc . 2021 on International Symposium on Symbolic

and Algebraic C omputation

345Œ351 (ACM, 2021).

20.

Heule, M. J., Kauers, M. & Seidl, M. New ways to multiply 3˜×˜3-matrices.

J. Symb. C omput.

1 04

, 899Œ916 ( 2021).

21.

Huber t, T. et˚al. Le arning and planning in complex action space s. In

International

C onference on Machine Le arning

4476Œ4486 (PMLR, 2021).

22.

Zhang, W. & Dietterich, T. G. A reinforcement le arning approach to job-shop scheduling.

In

International Joint C onferences on Ar ti˜icial Intelligence

Vol. 95, 1114Œ1120

(Morgan Kaufmann Publishers, 1995).

8,192

10,240

12,288

14,336

16,384

18,432

20,480

Matrix size

4.3%

8.5%

6.8%

10.7%

10.1%

13.3%

16.1%

19.6%

13.8%

16.6%

15.3%

17.9%

21.3%

23.9%

AlphaTensor

8,192

10,240

12,288

14,336

16,384

18,432

20,480

Matrix size

6.6%

10.3%

9.0%

12.4%

8.9%

13.9%

9.2%

13.4%

7.2%

11.2%

6.9%

12.3%

8.4%

13.9%

TP

U

GP

U

Benchmark devic

e

10.3%

2.8%

4.4%

8.5%

Optimized for TP

U

Optimized for GPU

Speed-up on Nvidia V100 GPU

Speed-up on TPU v2

Speed-up of tailor

ed agorithms

on both device

s

a

b

c

Strassen-squa

re

AlphaTensor

Strassen-squa

re

Fi g. 5 | S peed-ups of th e AlphaTensor-discovered algorithm.

a

,

b

, S peed-ups

(%) of the AlphaTenso r-d isc overed algorithms tailo red for a GPU (

a

) and a TPU

(

b

), optimized fo r a matrix multiplic ation of size 8,192˝×˝8,192 . S peed-ups a re

mea sured rel ative to standa rd (fo r exampl e , cuB LAS fo r the GPU) matrix

multiplic ation on the same ha rdwa re. S peed-ups a re rep o rted for va rio us

matrix sizes (despite optimizing the algo rithm only on one matrix size). We also

rep o rt the speed-up of theˆStrassen- s qua reˆ algo rithm. The median speed-up is

rep o rted over 200 ru ns. The standa rd dev iation over ru ns is <0.4 percen tage

p oin ts (see Supplemen ta ry Info rmation for mo re details).

c

, S peed-up of bo th

algo rithms (tailo red to a GPU and a TPU) benchma rked on bo th dev ices.

Nature

|

Vol˜˚˛

|

˜October˝˛˝˝

|

53

23.

Vaswani, A . Attention is all you need. In

International C onference on Neural Information

Proce ssing Systems

Vol 30, 5998Œ6008 (Curran Associates, 2017).

24.

Ho, J., Kalchbrenner, N., Weissenborn, D. & Salimans, T. Axial attention in

multidimensional transformers. Preprint at

https://arxiv.org/abs/1912.12180

( 2019).

25.

Drevet, C.-É ., Islam, M. N. & Schost, É . Optimiz ation technique s for small matrix

multiplication.

Theor. C omput. Sci.

41 2

, 2219Œ2236 ( 2011).

26.

Sedoglavic, A . A non-commut ative algorithm for multiplying ( 7˜×˜7) matrice s using 250

multiplications. Preprint at

https://arxiv.org/abs/1712.07935

( 2017 ).

27.

Batt aglia, P. W. et˚al. Relational inductive biases, deep le arning, and graph networks.

Preprint at

https://arxiv.org/abs/1806.01261

( 2018).

28.

Balog, M., van Merriënboer, B., Moitra, S., Li, Y. & Tarlow, D. Fast training of sparse graph

neural networks on dense hardware. Preprint at

https://arxiv.org/abs/1906.11786

( 2019).

29.

Ye, K. & Lim, L .-H. Fast structured matrix comput ations: tensor rank and CohnŒUmans

method.

Found. C omput. Math.

18

, 45Œ95 ( 2018).

30.

Bradbur y, J. et˚al. JAX: composable transformations of Python+NumPy programs.

GitHub

http://github.com/google/jax

( 2018).

31.

Benson, A . R. & Ballard, G. A framework for practical parallel fast matrix multiplication.

ACM SIGPL AN Not.

50

, 42Œ53 ( 2015).

32.

Huang, J., Smith, T. M., Henr y, G. M. & Van De Geijn, R. A . Strassen™s algorithm reloaded.

In

International Conference for High Per formance Computing, Networking, Storage and

Analy sis

690Œ701 (IEEE, 2016).

33.

Abadi, M. et˚al. Tensor ˛low: a system for large-scale machine le arning. In

USENIX

Symposium On Operating Systems Design And Implement ation

265Œ283 (USENIX,

2016).

Publisher ™s note

Springer Nature remains neutral with regard to jurisdictional claims in

published maps and institutional af˛iliations.

Open Acce ss

This ar ticle is licensed under a Cre ative Commons Attribution

4.0 International License, which permits use, sharing, adaptation, distribution

and reproduction in any medium or format, as long as you give appropriate

credit to the original author(s) and the source, provide a link to the Cre ative Commons license,

and indicate if change s were made. The image s or other third par ty material in this ar ticle are

included in the ar ticle™s Cre ative Commons license, unless indicated other wise in a credit line

to the material. If material is not included in the ar ticle™s Cre ative Commons license and your

intended use is not permitted by st atutor y regulation or exceeds the permitted use, you will

need to obt ain permission directly from the copyright holder. To view a copy of this license,

visit

http://creativecommons.org/license s/by/4.0/

.

© The Author(s) 2022

Methods

Tens orGame

TensorGame is played as follows. The star t position

˛

0

of the game

corresponds to the tensor

˜

representing the bilinear operation of

interest, expressed in some basis. In each step

t

of the game, the player

writes down three vectors (

u

(

t

)

,

v

(

t

)

,

w

(

t

)

), which specify the rank-1 tensor

u

(

t

)

˝˘˝

v

(

t

)

˝˘˝

w

(

t

)

, and the state of the game is updated by subtracting the

newly written down fac tor:

˛˛

uv

w

˘˘

.

(2

)

tt

tt

t

1

()

()

()

The game ends when the state reaches the zero tensor,

0

=

R

˛

. This

means that the factors written down throughout the game form a

factorization of the start tensor

˛

0

, that is,

=ˇ

˘˘

t

R

tt

t

0=

1

()

()

()

˛

uv

w

.

This factorization is then scored. For example, when optimizing for

asymptotic time complexity the score is 

R

, and when optimizing

for practical runtime the algorithm corresponding to the factorization

{

(,

,)

}

tt

t

t

R

()

()

()

=1

uv

w

is constructed (see Algorithm

1

) and then bench

-

marked on the fly (seeˆSupplementar y Information).

In prac tice, we also impose a limit

R

limit

on the maximum number of

moves in the game, so that a weak player is not stuck in unnecessarily

(or even inf initely) long games. When a game ends ˆbecause it has run

out of moves, a penalty score is given so that it is never advantageous

to deliberately exhaust the move limit. For example, when optimizing

for asymptotic time complexity, this penalty is derived from an upper

bound on the tensor rank of the final residual tensor

R

li

mi

t

˛

. This upper

bound on the tensor rank is obtained by summing the matrix ranks of

the slices of the tensor.

TensorGame over rings.

We say that the decomposition of

n

˜

in equa

-

tionˆ(

1

) is in a ring

˙

(def ining the arithmetic operations) if each of the

factors

u

(

t

)

,

v

(

t

)

and

w

(

t

)

has entries belonging to the set

˙

, and additions

and multiplications are interpreted according to

˙

. The tensor rank

depends, in general, on the ring. At each step of TensorGame, the ad

-

ditions and multiplications in equationˆ(

2

) are interpreted in

˙

. For

example, when working in

2

˜

, (in this case, the fac tors

u

(

t

)

,

v

(

t

)

and

w

(

t

)

live in

F

˝=˝{0,˝1}), a modulo 2 operation is applied after each state update

(equationˆ(

2

)).

We note that integer-valued decompositions

u

(

t

)

,

v

(

t

)

and

w

(

t

)

lead to

decompositions in arbitrar y rings

˙

. Hence, provided

F

only contains

integers, algorithms we find in standard arithmetic apply more gener

-

ally to any ring.

AlphaTensor

AlphaTensor builds on AlphaZero

1

and its extension Sampled

AlphaZero

21

, combining a deep neural network with a sample-based

MCTS search algorithm.

The deep neural network,

f

˛

(

s

)˝=˝(

˝

,˝

z

) parameterized by

˛

, takes as

input the current state

s

of the game and outputs a probability distribu

-

tion

˝

(

˛˝

s

) over actions and

z

(

˛˝

s

) over returns (sum of future rewards)

G

.

The parameters

˛

of the deep neural network are trained by reinforce

-

ment learning from self-play games and synthetic demonstrations.

Self-play games are played by actors, running a sample-based MCTS

search at every state

s

t

encountered in the game. The MCTS search

returns an improved probability distribution over moves from which

an action

a

t

is selected and applied to the environment. The sub-tree

under

a

t

is reused for the subsequent search at

s

t

+1

. At the end of the

game, a return

G

is obtained and the trajectory is sent to the learner to

update the neural network parameters

˛

. The distribution over returns

z

(

˛˝

s

t

) is learned through distributional reinforcement learning using

the quantile regression distributional loss

34

, and the network policy

˝

(

˛˝

s

t

) is updated using a KullbackŒLeibler divergence loss, to maximize

its similarity to the search policy for self-play games or to the next

action for synthetic demonstrations. We use the Adam optimizer

35

with decoupled weight decay

36

to optimize the parameters

˛

of the

neural net work.

Sample-based MCTS search.

The sample-based MCTS search is very

similar to the one described in Sampled AlphaZero. Specifically, the

search consists of a series of simulated trajectories of TensorGame that

are aggregated in a tree. The search tree thereforeˆconsists of nodes

representing states and edges representing actions. Each state-action

pair (

s

,˝

a

) stores a set of statistic s

Ns

aQ

sa

˝s

a

(,

),

(,

),

‹(

,)

, where

N

(

s

,˝

a

)

is the visit count,

Q

(

s

,˝

a

) is the ac tion value and

˝

sa

‹(

,)

is the empirical

policy probability. Each simulation traverses the tree from the root

state

s

0

until a leaf state

s

L

is reached by recursively selecting in each

state

s

an ac tion

a

that has not been frequently explored, has high em

-

pirical policy probability and high value. Concretely, actions within

the tree are selected by maximizing over the probabilistic upper con

-

f idence tree bound

21

,

37

ˇ

Qs

ac

s˝

sa

Ns

b

Ns

a

argmax

(,

)+

()

‹

(,

)

(,

)

1+

(,

)

,

a

b

where

c

(

s

) is an exploration fac tor controlling the influence of the

empirical policy

˝

sa

‹(

,)

relative to the values

Q

(

s

,˝

a

) as nodes are visited

more often. In addition, a transposition table is used to recombine

different action sequences if they reach the exact same tensor. This

can happen particularly often in TensorGame as actions are commuta

-

tive. Finally, when a leaf state

s

L

is reached, it is evaluated by the neural

net work, which returns

K

ac tions {

a

i

} sampled from

˝

(

a

˝

s

L

), alongside

the empirical distribution

ˇ

˝

as

˙

‹(

)=

Ki

aa

L

1

,

i

and a value

v

(

s

L

) con

-

structed from

z

(

˛˝

s

L

). Differently from AlphaZero and Sampled Alp

-

haZero, we chose

v

not to be the mean of the distribution of returns

z

(

˛˝

s

L

) as is usual in most reinforcement learning agents, but instead to

be a risk-seeking value, leveraging the facts that TensorGame is a deter

-

ministic environment and that we are primarily interested in finding

the best trajectory possible. The visit counts and values on the simu

-

lated trajectory are then updated in a backward pass as in Sampled

AlphaZero.

Policy improvement.

After simulating

N

(

s

) trajectories from state

s

using MCTS, the normalized visit counts of the actions at the root of

the search tree

N

(

s

,˝

a

)/

N

(

s

) form a sample-based improved policy. Dif

-

ferently from AlphaZero and S ampled AlphaZero, we use an adaptive

temperature scheme to smooth the normalized visit counts distribution

as some states can accumulate an order of magnitude more visits than

others because of sub -tree reuse and transposition table. Concretely,

we def ine the improved policy as

˝s

aN

sa

Ns

b

‹(

,)

=(

,)

/ˇ

(,

)

ˆs

b

ˆs

1/

()

1/

()

ˆ

where

ˆs

Ns

NN

N

()

=l

og

()

/l

og

if

>

and 1 otherwise, with

N

being a

hyperparameter. For training, we use

ˆ

˝

‹

directly as a target for the

network policy

˝

. For acting, we additionally discard all actions that

have a value lower than the value of the most visited ac tion, and sample

propor tionally to

˝

‹

ˆ

among those remaining high-value actions.

Learning one agent for multiple target tensors.

We train a single

agent to decompose the different tensors

nm

p

,,

˜

in a given arithmetic

(standard or modular). As the network works with fixed-size inputs, we

pad all tensors (with zeros) to the size of the largest tensor we consider

(

5

˜

, of size 25˝×˝25˝×˝25). At the beginning of each game, we sample uni

-

formly at random a target

nm

p

,,

˜

, and play TensorGame. Training a sin

-

gle agent on different targets leads to better results thanks to theˆtrans

-

fer bet ween targets. All our results repor ted in Fig.ˆ

3

are obtained using

multiple runs of this multi-target setting. We also train a single agent

to decompose tensors in both arithmetic s. Owing to learned transfer

between the two arithmetics, this agent discovers a different distribu

-

tion of algorithms (of the same ranks) in standard arithmetic than the

agent trained on standard arithmetic only, thereby increasing the over

-

all diversity of discovered algorithms.

Synthetic demonstrations.

The synthetic demonstrations buffer

contains tensor-factorization pairs, where the factorizations

uv

w

{

(,

,)

}

rr

r

r

R

()

()

()

=1

are first generated at random, after which the tensor

uv

w

=ˇ

˘˘

r

R

rr

r

=1

()

()

()

˝

is formed. We create a dataset containing

5 million such tensor-fac torization pairs. Each element in the fac tors

is sampled independently and identically distributed (i.i.d.) from a

given categorical distribution over

F

(all possible values that can be

taken). We discarded instances whose decompositions were clearly

suboptimal (contained a fac tor with

u

˝=˝

0

,

v

˝=˝

0

, or

w

˝=˝

0

).

In addition to these synthetic demonstrations, we further add to

the demonstration buffer previous games that have achieved large

scores to reinforce the good moves made by the agent in these games.

Change of basis.

The rank of a bilinear operation does not depend on

the basis in which the tensor representing it is expressed, and for any

invertible matrices

A

,

B

and

C

we have

R

an

k(

)=

Ra

nk

()

(,

,)

˜˜

AB

C

, where

(,

,)

AB

C

˜

is the tensor after change of basis given by

ˇˇ

ˇ

=.

(3

)

ij

k

a

S

b

S

c

S

ia

jb

kc

ab

c

(,

,)

=1

=1

=1

˜˜

AB

C

AB

C

Hence, exhibiting a rank-

R

decomposition of the matrix multiplica

-

tion tensor

n

˜

expressed in any basis proves that the product of two

n

˝×˝

n

matrices can be computed using

R

scalar multiplications. Moreo

-

ver, it is straightfor ward to conver t such a rank-

R

decomposition into

a rank-

R

decomposition in the canonical basis, thus yielding a practical

algorithm of the form shown in Algorithm

1

. We leverage this observa

-

tion by expressing the matrix multiplication tensor

n

˜

in a large number

of randomly generated bases (typically 100,000) in addition to the

canonical basis, and letting AlphaTensor play games in all bases in

parallel.

This approach has three appealing properties: (1) it provides a natural

exploration mechanism as playing games in different bases automati

-

cally injects diversity into the games played by the agent; (2) it exploits

properties of the problem as the agent need not succeed in all basesŠit

is sufficient to find a low-rank decomposition in any of the bases; (3)

it enlarges coverage of the algorithm space because a decomposition

with entries in a finite set

F

˝=˝{2,˝1,˝0,˝1,˝2} found in a different basis

need not have entries in the same set when converted back into the

canonical basis.

In full generality, a basis change for a 3D tensor of size

S

˝×˝

S

˝×˝

S

is

specified by three inver tible

S

˝×˝

S

matrices

A

,

B

and

C

. However, in our

procedure, we sample bases at random and impose two restrictions:

(1)

A

˝=˝

B

˝=˝

C

, as this performed better in early experiments, and (2)

unimodularity (

A

de

t

{

1,

+1

}

), which ensures that after converting

an integral factorization into the canonical basis it still contains integer

entries only (this is for representational convenience and numerical

stability of the resulting algorithm). See Supplementary Information

for the exact algorithm.

Signed permutations.

In addition to playing (and training on) games in

different bases, we also utilize a data augmentation mechanism when

-

ever the neural net work is queried in a new MCTS node. At acting time,

when the net work is queried, we transform the input tensor by applying

a change of basisŠwhere the change of basis matrix is set to a random

signed permutation. We then quer y the network on this transformed

input tensor, and finally invert the transformation in the network™s

policy predictions. Although this data augmentation procedure can

be applied with any generic change of basis matrix (that is, it is not

restricted to signed permutation matrices), we use signed permuta

-

tions mainly for computational efficiency. At training time, whenever

the neural network is trained on an (input, policy targets, value target)

triplet (Fig.ˆ

2

), we apply a randomly chosen signed permutation to

both the input and the policy targets, and train the network on this

transformed triplet. In prac tice, we sample 100 signed permutations

at the beginning of an experiment, and use them thereafter.

Action canonicaliz ation.

For any

˚

1

,˝

˚

2

,˝

˚

3

˝

˙

˝{1,˝+1} such that

˚

1

˚

2

˚

3

˝=˝1,

the actions (

˚

1

u

,˝

˚

2

v

,˝

˚

3

w

) and (

u

,˝

v

,˝

w

) are equivalent because they lead

to the same rank- one tensor (

˚

1

u

)˝˘˝(

˚

2

v

)˝˘˝(

˚

3

w

)˝=˝

u

˝˘˝

v

˝˘˝

w

. To pre

-

vent the network from wasting capacity on predicting multiple equiva

-

lent actions, during training we always present targets (

u

,˝

v

,˝

w

) for the

polic y head in a canonical form, def ined as having the f irst non-zero

element of

u

and the first non-zero element of

v

strictly positive. This is

well defined because

u

or

v

cannot be all zeros (if they are to be part of

a minimal rank decomposition), and for any (

u

,˝

v

,˝

w

) there are unique

˚

1

,˝

˚

2

,˝

˚

3

˝

˙

˝{1,˝+1} (with

˚

1

˚

2

˚

3

˝=˝1) that transform it into canonical form.

In case the network predicts multiple equivalent actions anyway, we

merge them together (summing their empirical polic y probabilities)

before inser ting them into the MCTS tree.

Training regime.

We train AlphaTensor on a TPU v3, with a total batch

size of 2,048. We use 64 TPU cores, and train for 600,000 iterations.

On the ac tor side, the games are played on standalone TPU v4, and we

use 1,600 actors. In practice, the procedure ˆtakes a week to converge.

Neural network

The architecture is composed of a torso, followed by a policy head that

predicts a distribution over actions, and a value head that predicts a dis

-

tribution of the returns from the current state (see Extended Data Fig.ˆ3).

Input.

The input to the net work contains all the relevant information

of the current state and is composed of a list of tensors and a list of

scalars. The most impor tant piece of information is the current 3D

tensor

˛

t

of size

S

˝×˝

S

˝×˝

S

. (For simplicity, in the description here we

assume that all the three dimensions of the tensor are equal in size. The

generalization to different sizes is straightforward.) In addition, the

model is given access to the last

h

ac tions (

h

being a hyperparameter

usually set to 7), represented as

h

rank-1 tensors that are concatenated

to the input. The list of scalars includes the time index

t

of the current

ac tion (where 0˝˝

t

˝<˝

R

limit

).

Torso.

The torso of the network is in charge of mapping both scalars and

tensors from the input to a representation that is useful to both policy

and value heads. Its architecture is based on a modification of transform

-

ers

23

, and its main signature is that it operates over three

S

˝×˝

S

grids pro

-

jected from the

S

˝×˝

S

˝×˝

S

input tensors. Each grid represents two out of

the three modes of the tensor. Defining the modes of the tensor as

,,

ˇ

˘

, the rows and columns of the first grid are associated to

ˇ

and

˘

,

respec tively, the rows and columns of the second grid are associated to



and

ˇ

, and the rows and columns of the third grid are associated to

˘

and



. Each element of each grid is a feature vec tor, and its initial value

is given by the elements of the input tensors along the grid™s missing

mode. These feature vectors are enriched by concatenating an

S

˝×˝

S

˝×˝1

linear projec tion from the scalars. This is followed by a linear layer pro

-

jec ting these feature vec tors into a 5 12- dimensional space.

The rest of the torso is a sequence of attention-based blocks with the

objective of propagating information between the three grids. Each of

those blocks has three stages, one for every pair of grids. In each stage,

the grids involved are concatenated, and axial attention

24

is performed

over the columns. It is noted that in each stage we perform in parallel

S

self-attention operations of 2

S

elements in each. The representation

sent to the policy head corresponds to the 3

S

2ˆ

512-dimensional feature

vec tors produced by the last layer of the torso. A detailed description

of the structure of the torso is specif ied in Extended Data Fig.ˆ4 (top)

and Appendix A .1.1 in Supplementar y Information.

Policy head.

The policy head uses the transformer architecture

23

to model an autoregressive policy. Factors are decomposed into

k

tokens of dimensionality

d

such that

k

˝×˝

d

˝=˝3

S

. The transformer con

-

ditions on the tokens already generated and cross-attends to the fea

-

tures produced by the torso. At training time, we use teacher-forcing,

that is, the ground truth actions are decomposed into tokens and

taken as inputs into the causal transformer in such a way that the

prediction of a token depends only on the previous tokens. At infer

-

ence time,

K

ac tions are sampled from the head. The feature repre

-

sentation before the last linear layer of the initial step (that is, the

only step that is not conditioned on the ground truth) is used as an

input to the value head, described below. Details of the architecture

are presented in Extended Data Fig.ˆ4 (centre) and Appendix A.1.2 in

Supplementar y Information.

Value head.

The value head is composed of a four-layer multilayer

perceptron whose last layer produces

q

outputs corresponding to the

,,

–

qq

q

q

1

2

3

2

2

1

2

quantiles. In this way, the value head predic ts the distri

-

bution of returns from this state in the form of values predicted for the

aforementioned quantiles

34

. At inference time, we encourage the agent

to be risk-seeking by using the average of the predicted values for quan

-

tiles over 75%. A detailed description of the value head is presented in

Extended Data Fig.ˆ4 (bottom) and Appendix A .1.3 in Supplementar y

Information.

Related work

The quest for efficient matrix multiplication algorithms started with

Strassen™s breakthrough in ref.

2

, which showed that one can multiply

2˝×˝2 matrices using 7 scalar multiplications, leading to an algorithm

of complexity

˚

n

()

2.

81

. This led to the development of a very active field

of mathematics attracting worldwide interest, which studies the asymp

-

totic complexity of matrix multiplication (see refs.

3

Œ

6

). So far, the best

known complexity for matrix multiplication is

n

()

2.

37

28

6

˚

(ref.

12

), which

improves over ref.

11

, and builds on top of fundamental results in the

field

8

Œ

10

. However, this does not yield practical algorithms, as such

approaches become advantageous only for astronomical matrix sizes.

Hence, a significant body of work aims at exhibiting explicit factoriza

-

tions of matrix multiplication tensors, as these factorizations provide

practical algorithms. After Strassen™s breakthrough showing that

˜

r

an

k(

)

7

2

, efficient algorithms for larger matrix sizes were

found

15

,

16

,

18

,

26

,

38

. Most notably, Laderman showed in ref.

15

that 3˝×˝3

matrix multiplications can be performed with 23 scalar multiplications.

In addition to providing individual low-rank fac torizations, an impor

-

tant research direction aims at understanding the space of matrix

multiplication algorithmsŠas opposed to exhibiting individual

low-rank factorizationsŠby studying the symmetry groups and diver

-

sity of factorizations (see ref.

5

and references therein). For example,

the symmetries of 2˝×˝2 matrix multiplication were studied in refs.

39

Œ

42

,

where Strassen™s algorithm was shown to be essentially unique. The

case of 3˝×˝3 was studied in ref.

43

, whereas a symmetric factorization

for all

n

is provided in ref.

44

.

On the computational front, continuous optimization has been the

main workhorse for decomposing tensors

17

,

45

,

46

, and in particular matrix

multiplication tensors. Such continuous optimization procedures (for

example, alternating least squares), however, yield approximate solu

-

tions, which correspond to inexact matrix multiplication algorithms

with floating point operations. To circumvent this issue, regularization

procedures have been proposed, such as ref.

18

, to extrac t exact decom

-

positions. Unfor tunately, such approaches often require ˆsubstantial

human inter vention and exper tise to decompose large tensors. A dif

-

ferent line of attack was explored in refs.

47

,

48

, based on learning the

continuous weights ofa t wo-layer net wo rk  th at  m imi cs  th e st ru ctu re

of the matrix multiplication operation. This method, which is trained

through supervised learning of matrix multiplication examples, finds

approximate solutions to 2˝×˝2 and 3˝×˝3 matrix multiplications.

In ref.

48

, a quantization procedure is fur ther used to obtain an exac t

decomposition for 2˝×˝2. Unlike continuous optimization-based

approaches, AlphaTensor directly produces algorithms from the

desired set of valid algorithms, and is flexible in that it allows us to

optimize a wide range of (even non-differentiable) objectives. This

unlocks tackling broader settings (for example, optimization in finite

f ields, optimization of runtime), as well as larger problems (for exam

-

ple,

˜

4

and

˜

5

) than those previously considered. Different from con

-

tinuous optimization, a boolean satisfiability (SAT)ˆbasedˆ formulation

of the problem of decomposing 3˝×˝3 matrix multiplication was

recently proposed in ref.

20

, which adds thousands of new decompo

-

sitions of rank 23 to the list of known 3˝×˝3 factorizations. The approach

relies on a state-of-the-art SAT solving procedure, where several

assumptions and simplifications are made on the factorizations to

reduce the search space. As is, this approach is, however, unlikely to

scale to larger tensors, as the search space grows very quickly with

the size.

On the practical implementation front, ref.

31

proposed several ideas

to speed up implementation of fast matrix multiplication algorithms

on central processing unitsˆ(CPUs). Different fast algorithms are then

compared and benchmarked, and the potential speed-up of such algo

-

rithms is shown against standard multiplication. Other works focused

on getting the maximal performance out of a particular fast matrix

multiplication algorithm (Strassen™s algorithm with one or t wo levels

of recursion) on a CPU

32

or a GPU

49

. These works show that, despite

popular belief, such algorithms are of practical value. We see writing

a custom low-level implementation of a given algorithm to be distinct

from the focus of this paperŠdeveloping new efficient algorithmsŠand

we believe that the algorithms we discovered can further benefit from

a more eff icient implementation by exper ts.

Beyond matrix multiplication and bilinear operations, a growing

amount of research studies the use of optimization and machine learn

-

ing to improve the efficiency of computational operations. There

are three levels of abstractions at which this can be done: (1) in the

hardware design, for example, chip floor planning

50

, (2) at the hard

-

wareŒsoftware interface, for example, program super-optimization

of a reference implementation for specific hardware

51

, and (3) on the

algorithmic level, for example, program induc tion

52

, algorithm selec

-

tion

53

or meta-learning

54

. Our work focuses on the algorithmic level of

abstraction, although AlphaTensor is also flexible to discover efficient

algorithms for specific hardware. Different from previous works, we

focus on discovering matrix multiplication algorithms that are prov

-

ably correct, without requiring initial reference implementations. We

conclude by relating our work broadly to existing reinforcement learn

-

ing methods for scientif ic discover y. Within mathematic s, reinforce

-

ment learning was applied, for example, to theorem proving

55

Œ

58

, and

to finding counterexamples refuting conjectures in combinatorics and

graph theory

59

. Reinforcement learning was further shown to be useful

in many areas in science, such as molecular design

60

,

61

and synthesis

62

and optimizing quantum dynamic s

63

.

Dat a availabilit y

The data used to train the system were generated synthetically accord

-

ing to the procedures explained in the paper. The algorithms discov

-

ered by AlphaTensor are available for download at

https://github.com/

deepmind/alphatensor

.

Code availability

An interac tive notebook with code to check the non-equivalence of algo

-

rithms is provided. Moreover, the fast algorithms from the ‚Algorithm

discover yˆresults™ sec tionˆon a GPU and a TPU are provided. These are

available for download at

https://github.com/deepmind/alphatensor

.

A full description of the AlphaZero algorithm that this work is based

on is available in ref.

1

, and the specific neural network architecture we

use is described using pseudocode in the Supplementary Information.

34.

Dabney, W., Rowland, M., Bellemare, M. & Munos, R. Distributional reinforcement le arning

with quantile regre ssion. In

AAAI C onference on Ar ti˜icial Intelligence

Vol. 32, 2892Œ2901

(AAAI Pre ss, 2018).

35.

Kingma, D. P., & Ba, J. Adam: a method for stochastic optimization. In

International

C onference on Le arning Represent ations (ICLR)

( 2015).

36.

Loshchilov, I. & Hutter, F. Decoupled weight decay regulariz ation. In

International

C onference on Le arning Represent ations (ICLR)

( 2019).

37.

Silver, D. et˚al. Mastering the game of Go with deep neural networks and tree se arch.

Nature

52 9

, 484Œ489 ( 2016).

38.

Sedoglavic, A . A non-commut ative algorithm for multiplying 5x5 matrice s using 99

multiplications. Preprint at

https://arxiv.org/abs/1707.06860

( 2017 ).

39.

de Groote, H. F. On varietie s of optimal algorithms for the comput ation of bilinear

mappings II. optimal algorithms for 2˜×˜2-matrix multiplication.

Theor. C omput. Sci.

7

,

127Œ148 (1978).

40.

Burichenko, V. P. On symmetrie s of the Strassen algorithm. Preprint at

https://arxiv.org/

a b s/14 08.6273

( 2014).

41.

Chiantini, L., Ikenmeyer, C., Landsberg, J. M. & Ottaviani, G. The geometry of rank

decompositions of matrix multiplication I: 2˜×˜2 matrice s.

Exp. Math.

28

, 322Œ327 ( 2019).

42.

Grochow, J. A . & Moore, C. De signing Strassen™s algorithm. Preprint at

https://arxiv.org/

a b s/1 708.0 93 98

( 2017 ).

43.

Ballard, G., Ikenmeyer, C., Landsberg, J. M. & Ryder, N. The geometr y of rank

decompositions of matrix multiplication II: 3˜×˜3 matrice s.

J. Pure Appl. Algebra

2 23

,

3205Œ3224 ( 2019).

44.

Grochow, J. A . & Moore, C. Matrix multiplication algorithms from group orbits. Preprint at

https://arxiv.org/abs/1612.01527

( 2016).

45.

Kolda, T. G. & Bader, B. W. Tensor decompositions and applications.

SIAM Rev.

51

,

455Œ500 ( 2009).

46.

Bernardi, A ., Brachat, J., Comon, P. & Mourrain, B. General tensor decomposition,

moment matrice s and applications.

J. Symb. C omput.

52

, 51Œ71 ( 2013).

47.

Elser, V. A network that le arns Strassen multiplication.

J. Mach. Learn. Res .

17

, 3964Œ3976

( 2016).

48.

Tschannen, M., Khanna, A . & Anandkumar, A , StrassenNets: deep learning with a

multiplication budget. In

International C onference on Machine Le arning

4985Œ4994

(PMLR, 2018).

49.

Huang, J., Yu, C. D. & Geijn, R. A . V. D. Strassen™s algorithm reloaded on GPUs.

ACM Trans.

Math. Sof tw.

46

, 1Œ22 ( 2020).

50.

Mirhoseini, A. et˚al. A graph placement methodology for fast chip de sign.

Nature

5 94

,

207Œ212 ( 2021).

51.

Bunel, R., De smaison, A ., Kohli, P., Torr, P. H. & Kumar, M. P. Learning to superoptimize

programs. In

International Conference on Le arning Repre sent ations (ICLR)

( 2017 ).

52.

Li, Y., Gimeno, F., Kohli, P. & Vinyals, O. Strong generaliz ation and ef˛iciency in neural

programs. Preprint at

https://arxiv.org/abs/2007.03629

(2020).

53.

Lagoudakis, M. G. et˚al. Algorithm selection using reinforcement le arning. In

International

C onference on Machine Le arning

511Œ518 (Morgan Kaufmann Publishers, 2000).

54.

Schmidhuber, J.

Evolutionar y Principle s in Self-Re ferential Le arning. On Le arning now to

Le arn: The Met a-Met a-Meta...-Hook

. Diploma the sis, Technische Univ. Munchen (1987 ).

55.

Kalisz yk, C., Urban, J., Michalewski, H. & Olıák, M. Reinforcement le arning of theorem

proving. In

International Conference on Neural Information Proce ssing Sy stems

8836Œ8847 (Curran Associate s, 2018).

56.

Piotrowski, B. & Urban, J. ATPboost: learning premise selection in binar y setting with ATP

feedback. In

International Joint Conference on Automated Reasoning

566Œ574 (Springer,

2018).

57.

Bansal, K., Loos, S., Rabe, M., Szegedy, C. & Wilcox, S. HOList: an environment for

machine le arning of higher order logic theorem proving. In

International C onference on

Machine Le arning

454Œ463 (PMLR, 2019).

58.

Zombori, Z., Urban, J. & Brown, C. E . Prolog technology reinforcement le arning prover.

In

International Joint C onference on Automated Re asoning

489Œ507 (Springer, 2020).

59.

Wagner, A . Z. Constructions in combinatorics via neural networks. Preprint at

https://

arxiv.org/abs/2104.14516

( 2021).

60.

Popova, M., Isayev, O. & Tropsha, A . Deep reinforcement le arning for de˚novo drug

de sign.

Sci.

A d v.

4

, e aap7885 ( 2018).

61.

Zhou, Z., Ke arne s, S., Li, L ., Zare, R. N. & Riley, P. Optimiz ation of molecule s via deep

reinforcement le arning.

Sci. Rep.

9

, 10752 ( 2019).

62.

Segler, M. H., Preuss, M. & Waller, M. P. Planning chemical synthese s with deep neural

networks and symbolic AI.

Nature

555

, 604Œ610 ( 2018).

63.

Dalgaard, M., Mot zoi, F., Sørensen, J. J. & Sherson, J. Global optimiz ation of quantum

dynamics with AlphaZero deep exploration.

npj Quantum Inf.

6

, 6 ( 2020).

64.

Fast matrix multiplication algorithms cat alogue.

Université de Lille

https://fmm.univ-

lille.fr/

( 2021).

Acknowledgements

We thank O. Fawzi, H. Fawzi, C. Ikenmeyer, J. Ellenberg, C. Umans and

A . Wigderson for the inspiring˚discussions on the use of machine le arning for maths; A. Davie s,

A . Gaunt, P. Mudigonda, R. Bunel and O. Ronneberger for their advice on early drafts of the

paper; A . Ruderman, M. Bauer, R. Leblond, R. Kabra and B. Winckler for par ticipating in a

hackathon at the e arly st age s of the project; D. Visentin, R. Tanburn and S. Nour y for sharing

their exper tise on TPUs; P.˚Wang and R.˚Zhao for their help on benchmarking algorithms; ˚

G. Holland, A . Pierce, N. Lamber t and C. Meyer for assist ance coordinating the re se arch; and

our colle agues at DeepMind for encouragement and support.

Author contributions

A .F. conceived the project, with suppor t from B.R.-P. and P.K.; T.H., A .H.

and J.S. developed the initial AlphaZero codebase, and B.R.-P., M. Balog, A .F., A .N., F.J.R.R. and

G.S. developed an e arly super vised network prototype. A .H., T.H., B.R.-P., M. Barekat ain and

J.S. de signed the network architecture used in the paper. T.H., J.S., A .H., M. Barekatain, A .F.,

M. Balog and F.J.R.R. developed the tensor decomposition environment and dat a generation

pipeline, and A .H., T.H., M. Barekat ain, M. Balog, B.R.-P., F.J.R.R. and A .N. analysed the

experiment al re sults and algorithms discovered by AlphaTensor. A .N., A .F. and T.H. developed

the benchmarking pipeline and experiments, and B.R.-P., F.J.R.R. and A .N. extended the

approach to structured tensors. A.F., B.R.-P., G.S. and A .N. proved the re sults in the paper.

D.S., D.H. and P.K. contributed technical advice and ide as. A .F., M. Balog, B.R.-P., F.J.R.R., A .N.

and T.H. wrote the paper. The se authors contributed equally, and are listed alphabetically by last

name after the corre sponding author: A.F., M. Balog, A .H., T.H., B.R.-P.˚These authors contributed

equally, and are listed alphabetically by last name: M. Barekat ain, A .N., F.J.R.R., J.S. and G.S.

Competing interest s

The authors of the paper are planning to ˛ile a patent application relating

to subject matter cont ained in this paper in the name of DeepMind Technologie s Limited.

Additional information

Supplement ar y information

The online version cont ains supplement ar y material available at

https://doi.org/10.1038/s41586-022-05172-4

.

Corre spondence and reque st s for materials

should be addressed to Alhussein Fawzi.

Peer review information

Nature

thanks Grey Ballard, Jordan Ellenberg, Lek-Heng Lim, Michael

Littman and the other, anonymous, reviewer(s) for their contribution to the peer review of this

work.

Reprint s and permissions information

is available at

http://w ww.nature.com/reprints

.

Ex tende d Data Fi g. 1 | Algorithm for multiplying 4˚×˚4 matrice s in modular arithmetic (

2

˜

) with 47 multiplication s.

This outperforms the two-level Stra ssen™s

algo rithm, which involves 7

2

˝=˝49 multiplic ations.

Ex ten de d Data Fi g. 2 | Algorithm for multiplying 4 × 5 by 5 × 5 matrice s in standard arithmetic with 76 multiplication s.

This o utperfo rms the prev io usly best

known a lgo rithm, which involves 80 multiplic ations.

Ex ten de d Data Fi g. 3 | AlphaTen sor™s network architecture.

The netwo rk

takes as input the list of tenso rs c ontaining the curren t state and prev io us

histo ry of actions, and a list of scal a rs, such a s the time index of the curren t

action. It pro duces two kinds of o utputs: one represen ting the value , and the

o ther inducing a distribution over the action space f rom which we c an sample

f rom. The a rchitecture of the netwo rk is acc o rd ingly designed to have a

c ommon torso, and two head s, the value and the p olicy heads.

c

is set to 512 in

all experiments.

Ex ten de d Data Fi g. 4 | Detaile d view of AlphaTen sor™s arch itecture, included torso, polic y an d value h ead .

We refer to Algo rithms A .1-A .11 in Supplemen ta ry

Info rmation fo r the details of each c omp onen t.

E xtended Data Table 1 | Rank re sult s obt ained by combining decompositions˜(in st andard arithmetic)

The t able shows the case s where we were able to obt ain an improvement over state-of-the-ar t, for tensors

˜

nm

p

,,

(with

n

,˜

m

,˜

p

˝12). The recipe column indicate s the low-level matrix multiplica

-

tion algorithms used to build the corre sponding factoriz ation.

ˆ

n

,˜

m

,˜

p

ˇ

denote s the be st known bound on the rank of

˜

nm

p

,,

; see Appendix H in Supplement ary Information for more det ails. For

tensors that were directly decomposed by AlphaTensor, the recipe shows a star mark, e.g.

ˆ

3,˜4,˜5

ˇ

*

. All the factoriz ations are made available.

E xtended Data Table 2 | Re sult of applying AlphaTensor to the tensor repre senting the cyclic convolution operation

AlphaTensor ˛inds the discrete Fourier matrix (DFT ) and the inverse DFT matrix in ˛inite ˛ields. The ˛igure shows the decompositions found by AlphaTensor of the

n

˜×˜

n

˜×˜

n

tensor repre senting the

cyclic convolution of two vectors, for three different value s of

n

in the ˛inite ˛ield of order 17. The action space, characterized by the number of possible factor triplets {

u

(

r

)

,˜

v

(

r

)

,˜

w

(

r

)

}, is thus 17

3

n

,

which is of the order of 10

29

for

n

˜=˜8. De spite the huge action space, AlphaTensor ˛inds the optimal rank-

n

decompositions for the three value s of

n

. The factors in the ˛igure are st acked ver tically,

i.e.,

U

˜=˜[

u

(1)

,˜–,˜

u

(

n

)

]. For e ase of visualiz ation, the factor entrie s have been expre ssed in terms of powers of an

n

-th primitive root of unity in the ˛inite ˛ield. Within e ach column, e ach colour

uniquely represents one element of the ˛ield (e.g., for the column

n

˜=˜4, we have depicted in grey 4

0

˜=˜4

4

˜=˜4

˙4

˜=˜1). By inspecting the patterns in the decompositions, one could extrapolate the

re sults for other values of

n

and other ˛ields. Indeed, the factors

u

(

r

)

and

v

(

r

)

corre spond to the DFT coef˛icients, since

==

u

vz

kr

k

r

k

r

()

()

, where as the factors

w

(

r

)

corre spond to the inverse DFT, since

wz

n

/

kr

k

r

()

=



for 0˝

k

,˜

r

˜<˜

n

, where

z

is an

n

-th primitive root of unity (i.e.,

z

n

˜=˜1 and

z

j

˜ˆ˜1 for any 1˝

j

˜<˜

n

).