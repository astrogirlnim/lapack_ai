Article
Discovering faster matrix multiplication
algorithms with reinforcement learning
Alhussein Fawzi1,2 ✉, Matej Balog1,2
, Aja Huang1,2
, Thomas Hubert1,2
,
Bernardino Romera-Paredes1,2
, Mohammadamin Barekatain1
, Alexander Novikov1
,
Francisco J. R. Ruiz1
, Julian Schrittwieser1
, Grzegorz Swirszcz1
, David Silver1
, Demis Hassabis1
& Pushmeet Kohli1
Improving the efficiency of algorithms for fundamental computations can have a
widespread impact, as it can affect the overall speed of a large amount of computations.
Matrix multiplication is one such primitive task, occurring in many systems—from
neural networks to scientific computing routines. The automatic discovery of
algorithms using machine learning offers the prospect of reaching beyond human
intuition and outperforming the current best human-designed algorithms. However,
automating the algorithm discovery procedure is intricate, as the space of possible
algorithms is enormous. Here we report a deep reinforcement learning approach
based on AlphaZero1 for discovering efficient and provably correct algorithms for the
multiplication of arbitrary matrices. Our agent, AlphaTensor, is trained to play a
single-player game where the objective is finding tensor decompositions within a
finite factor space. AlphaTensor discovered algorithms that outperform the state-
of-the-art complexity for many matrix sizes. Particularly relevant is the case of 4 × 4
matrices in a finite field, where AlphaTensor’s algorithm improves on Strassen’s two-
level algorithm for the first time, to our knowledge, since its discovery 50 years ago2
.
We further showcase the flexibility of AlphaTensor through different use-cases:
algorithms with state-of-the-art complexity for structured matrix multiplication and
improved practical efficiency by optimizing matrix multiplication for runtime on
specific hardware. Our results highlight AlphaTensor’s ability to accelerate the
process of algorithmic discovery on a range of problems, and to optimize for different
criteria.
We focus on the fundamental task of matrix multiplication, and use
deep reinforcement learning (DRL) to search for provably correct and
efficient matrix multiplication algorithms. This algorithm discovery
process is particularly amenable to automation because a rich space of
matrix multiplication algorithms can be formalized as low-rank decom-
positions of a specific three-dimensional (3D) tensor 2
, called the matrix
multiplication tensor3–7
. This space of algorithms contains the stand-
ard matrix multiplication algorithm and recursive algorithms such as
Strassen’s2
, as well as the (unknown) asymptotically optimal algorithm.
Although an important body of work aims at characterizing the com-
plexity of the asymptotically optimal algorithm8–12
, this does not yield
practical algorithms 5
. We focus here on practical matrix multiplication
algorithms, which correspond to explicit low-rank decompositions of
the matrix multiplication tensor. In contrast to two-dimensional matri-
ces, for which efficient polynomial-time algorithms computing the rank
have existed for over two centuries13
, finding low-rank decompositions
of 3D tensors (and beyond) is NP-hard 14 and is also hard in practice.
In fact, the search space is so large that even the optimal algorithm
for multiplying two 3 × 3 matrices is still unknown. Nevertheless, in a
longstanding research effort, matrix multiplication algorithms have
been discovered by attacking this tensor decomposition problem using
human search 2,15,16
, continuous optimization17–19 and combinatorial
search20
. These approaches often rely on human-designed heuristics,
which are probably suboptimal. We instead use DRL to learn to recog-
nize and generalize over patterns in tensors, and use the learned agent
to predict efficient decompositions.
We formulate the matrix multiplication algorithm discovery pro-
cedure (that is, the tensor decomposition problem) as a single-player
game, called TensorGame. At each step of TensorGame, the player
selects how to combine different entries of the matrices to multiply.
A score is assigned based on the number of selected operations required
to reach the correct multiplication result. This is a challenging game
with an enormous action space (more than 1012 actions for most inter-
esting cases) that is much larger than that of traditional board games
such as chess and Go (hundreds of actions). To solve TensorGame and
find efficient matrix multiplication algorithms, we develop a DRL agent,
AlphaTensor. AlphaTensor is built on AlphaZero1,21
, where a neural net-
work is trained to guide a planning procedure searching for efficient
matrix multiplication algorithms. Our framework uses a single agent
to decompose matrix multiplication tensors of various sizes, yielding
https://doi.org/10.1038/s41586-022-05172-4
Received: 2 October 2021
Accepted: 2 August 2022
Published online: 5 October 2022
Open access
Check for updates
1
DeepMind, London, UK. 2
These authors contributed equally: Alhussein Fawzi, Matej Balog, Aja Huang, Thomas Hubert and Bernardino Romera-Paredes. ✉e-mail: afawzi@deepmind.com
48 | Nature | Vol 610 | 6 October 2022Article
transfer of learned decomposition techniques across various tensors.
To address the challenging nature of the game, AlphaTensor uses a
specialized neural network architecture, exploits symmetries of the
problem and makes use of synthetic training games.
AlphaTensor scales to a substantially larger algorithm space than
what is within reach for either human or combinatorial search. In fact,
AlphaTensor discovers from scratch many provably correct matrix
multiplication algorithms that improve over existing algorithms in
terms of number of scalar multiplications. We also adapt the algo-
rithm discovery procedure to finite fields, and improve over Strassen’s
two-level algorithm for multiplying 4 × 4 matrices for the first time, to
our knowledge, since its inception in 1969. AlphaTensor also discovers a
diverse set of algorithms—up to thousands for each size—showing that
the space of matrix multiplication algorithms is richer than previously
thought. We also exploit the diversity of discovered factorizations to
improve state-of-the-art results for large matrix multiplication sizes.
Through different use-cases, we highlight AlphaTensor’s flexibility
and wide applicability: AlphaTensor discovers efficient algorithms
for structured matrix multiplication improving over known results,
and finds efficient matrix multiplication algorithms tailored to spe-
cific hardware, by optimizing for actual runtime. These algorithms
multiply large matrices faster than human-designed algorithms on
the same hardware.
Algorithms as tensor decomposition
As matrix multiplication (A, B) ↦ AB is bilinear (that is, linear in both
arguments), it can be fully represented by a 3D tensor: see Fig. 1a for
how to represent the 2 × 2 matrix multiplication operation as a 3D ten-
sor of size 4 × 4 × 4, and refs. 3,5,7 for more details. We write nT for the
tensor describing n × n matrix multiplication. The tensor Tn is fixed
(that is, it is independent of the matrices to be multiplied), has entries
in {0, 1}, and is of size n2 × n2 × n2 . More generally, we use n m p, ,T to
describe the rectangular matrix multiplication operation of size n × m
with m × p (note that =n n n n, ,T T ). By a decomposition of Tn into R
rank-one terms, we mean
T ∑= ⊗ ⊗ , (1)n
r
R
r r r
=1
( ) ( ) ( )
u v w
where ⊗ denotes the outer (tensor) product, and u(r) , v(r) and w(r) are all
vectors. If a tensor T can be decomposed into R rank-one terms, we say
the rank of T is at most R, or RRank ( ) ≤T . This is a natural extension
from the matrix rank, where a matrix is decomposed into ∑ ⊗r
R r r
=1 ( ) ( )
u v .
A decomposition of Tn into R rank-one terms provides an algorithm
for multiplying arbitrary n × n matrices using R scalar multiplications
(see Algorithm 1). We refer to Fig. 1b,c for an example algorithm mul-
tiplying 2 × 2 matrices with R = 7 (Strassen’s algorithm).
Crucially, Algorithm 1 can be used to multiply block matrices. By using
this algorithm recursively, one can multiply matrices of arbitrary size, with
the rank R controlling the asymptotic complexity of the algorithm. In par-
ticular, N × N matrices can be multiplied with asymptotic complexity
O N( )Rlog ( )n ; see ref. 5 for more details.
DRL for algorithm discovery
We cast the problem of finding efficient matrix multiplication algo-
rithms as a reinforcement learning problem, modelling the environ-
ment as a single-player game, TensorGame. The game state after step
t is described by a tensor St , which is initially set to the target tensor
we wish to decompose: S T= n0 . In each step t of the game, the player
selects a triplet (u(t)
, v(t)
, w(t)
), and the tensor St is updated by subtract-
ing the resulting rank-one tensor: ← − ⊗ ⊗t t
t t t
−1
( ) ( ) ( )
S S u v w . The goal
of the player is to reach the zero tensor 0=tS by applying the smallest
number of moves. When the player reaches the zero tensor, the
sequence of selected factors satisfies u v wT = ∑ ⊗ ⊗n t
R t t t
=1
( ) ( ) ( ) (where
R denotes the number of moves), which guarantees the correctness of
the resulting matrix multiplication algorithm. To avoid playing unnec-
essarily long games, we limit the number of steps to a maximum value,
Rlimit.
For every step taken, we provide a reward of −1 to encourage finding
the shortest path to the zero tensor. If the game terminates with a
non-zero tensor (after Rlimit steps), the agent receives an additional
terminal reward equal to γ− ( )R limit
S , where γ( )R limit
S is an upper bound
on the rank of the terminal tensor. Although this reward optimizes for
rank (and hence for the complexity of the resulting algorithm), other
reward schemes can be used to optimize other properties, such as
practical runtime (see ‘Algorithm discovery results’). Besides, as our
aim is to find exact matrix multiplication algorithms, we constrain
{u(t) , v(t) , w(t) } to have entries in a user-specified discrete set of coeffi-
cients F (for example, F = {−2, −1, 0, 1, 2}). Such discretization is com-
mon practice to avoid issues with the finite precision of floating
points 15,18,20
.
To play TensorGame, we propose AlphaTensor (Fig. 2), an agent based
on AlphaZero1
, which achieved tabula rasa superhuman performance
in the classical board games of Go, chess and shogi, and on its extension
to handle large action spaces Sampled AlphaZero 21 . Similarly to
AlphaZero, AlphaTensor uses a deep neural network to guide a Monte
c1 c2
c3 c4
= a1 a2
a3 a4
· b1 b2
b3 b4
a
U =
1 0 1 0 1 0
0 0 0 0 1 1
0 1 0 0 0 0
1 1 0 1 0 –1
V =
1 0
0 0 0
0 1 1
1 0 1
W =
1 0 0 1 1
0 0 1 0 1 0
0 1 0 1 0 0
1 0 0a1 a2 a3 a4
b1
b2
b3
b4
c1
c3
c2
c4
b c
–1
0
1
0
1 0
0 1
0 0
0 –1
–1 0 1
0 1
0 0
1 0
–1 1 0
0
0
0
1
–1
m1 = ( a1 + a4 )( b1 + b4 )
m2 = ( a3 + a4 ) b1
m3 = a1 ( b2 – b4 )
m4 = a4 ( b3 – b1 )
m5 = ( a1 + a2 ) b4
m6 = ( a3 – a1 )( b1 + b2 )
m7 = ( a2 – a4 )( b3 + b4 )
c1 = m1 + m4 – m5 + m7
c2 = m3 + m5
c3 = m2 + m4
c4 = m1 – m2 + m3 + m6
Fig. 1 | Matrix multiplication tensor and algorithms. a, Tensor T2 representing
the multiplication of two 2 × 2 matrices. Tensor entries equal to 1 are depicted
in purple, and 0 entries are semi-transparent. The tensor specifies which entries
from the input matrices to read, and where to write the result. For example,
as c1 = a1b1 + a2b3 , tensor entries located at (a1 , b1 , c1 ) and (a2 , b3 , c1 ) are set to 1.
b, Strassen's algorithm 2 for multiplying 2 × 2 matrices using 7 multiplications.
c, Strassen's algorithm in tensor factor representation. The stacked factors
U, V and W (green, purple and yellow, respectively) provide a rank-7
decomposition of 2T (equation (1)). The correspondence between arithmetic
operations (b) and factors (c) is shown by using the aforementioned colours.
Nature | Vol 610 | 6 October 2022 | 49
Carlo tree search (MCTS) planning procedure. The network takes as
input a state (that is, a tensor tS to decompose), and outputs a policy
and a value. The policy provides a distribution over potential actions.
As the set of potential actions (u(t)
, v(t)
, w(t)
) in each step is enormous,
we rely on sampling actions rather than enumerating them 21,22
. The
value provides an estimate of the distribution z of returns (cumulative
reward) starting from the current state tS . With the above reward
scheme, the distribution z models the agent’s belief about the rank of
the tensor St . To play a game, AlphaTensor starts from the target tensor
( nT ) and uses the MCTS planner at each step to choose the next action.
Finished games are used as feedback to the network to improve the
network parameters.
Overcoming the challenges posed by TensorGame—namely, an enor-
mous action space, and game states described by large 3D tensors
representing an abstract mathematical operation—requires multiple
advances. All these components, described briefly below, substantially
improve the overall performance over a plain AlphaZero agent (see
Methods and Supplementary Information for details).
Neural network architecture
We propose a transformer-based 23 architecture that incorporates
inductive biases for tensor inputs. We first project the S × S × S input
tensor into three S × S grids of feature vectors by using linear layers
applied to the three cyclic transpositions of the tensor. The main part of
the model comprises a sequence of attention operations, each applied
to a set of features belonging to a pair of grids (Extended Data Figs. 3
and 4). This generalizes axial attention 24 to multiple grids, and is both
more efficient and yields better results than naive self-attention. The
proposed architecture, which disregards the order of rows and columns
in the grids, is inspired by the invariance of the tensor rank to slice
reordering. The final feature representation of the three matrices is
passed both to the policy head (an autoregressive model) and the value
head (a multilayer perceptron).
Synthetic demonstrations
Although tensor decomposition is NP-hard, the inverse task of con-
structing the tensor from its rank-one factors is elementary. Hence,
we generate a large dataset of tensor-factorization pairs (synthetic
demonstrations) by first sampling factors u v w{( , , )}r r r
r
R( ) ( ) ( )
=1 at random,
and then constructing the tensor = ∑ ⊗ ⊗r
R r r r
=1 ( ) ( ) ( )
D u v w . We train the
network on a mixture of supervised loss (that is, to imitate synthetic
demonstrations) and standard reinforcement learning loss (that is,
learning to decompose a target tensor nT ) (Fig. 2). This mixed training
strategy—training on the target tensor and random tensors— substan-
tially outperforms each training strategy separately. This is despite
randomly generated tensors having different properties from the tar-
get tensors.
Change of basis
nT (Fig. 1a) is the tensor representing the matrix multiplication bilinear
operation in the canonical basis. The same bilinear operation can be
expressed in other bases, resulting in other tensors. These different
Algorithm 1
A meta-algorithm parameterized by =u v w{ , , }r r r( ) ( ) ( )
r
R
1 for computing
the matrix product C = AB. It is noted that R controls the number of
multiplications between input matrix entries.
Parameters: =u v w{ , , }r r r( ) ( ) ( )
r
R
1: length-n2 vectors such that
Tn
r r r( ) ( ) ( )
r
R
1= ∑ ⊗ ⊗= u v w
Input: A, B: matrices of size n × n
Output: C = AB
(1) for r = 1, ..., R do
(2) ← + + + + m u a u a v b v b( ) ( )r n n1 1
r
n
r r
n
r
1
( ) ( )
1
( ) ( )
2 22 2
(3) for i = 1, ..., n2 do
(4) c w m w mi R1i i
R(1) ( )
← + +
return C
Change of basis
Pre-generated
synthetic
demonstrations
Played games
buffer
Played
game
Sample
random state
Neural network
Policy head
Value head
Acting
...
LearningUpdated
model
Network inputTraining labels
(u, v, w)
(u(1), v(1) , w(1) ) (u(2), v(2) , w(2) ) (u(3) , v(3) , w(3) )
Fig. 2 | Overview of AlphaTensor. The neural network (bottom box) takes
as input a tensor St , and outputs samples (u, v, w) from a distribution
over potential next actions to play, and an estimate of the future returns
(for example, of S−Rank ( )t ). The network is trained on two data sources:
previously played games and synthetic demonstrations. The updated network
is sent to the actors (top box), where it is used by the MCTS planner to generate
new games.
50 | Nature | Vol 610 | 6 October 2022Article
tensors are equivalent: they have the same rank, and decompositions
obtained in a custom basis can be mapped to the canonical basis, hence
obtaining a practical algorithm of the form in Algorithm 1. We leverage
this observation by sampling a random change of basis at the beginning
of every game, applying it to Tn, and letting AlphaTensor play the game
in that basis (Fig. 2). This crucial step injects diversity into the games
played by the agent.
Data augmentation
From every played game, we can extract additional tensor-factorization
pairs for training the network. Specifically, as factorizations are
order invariant (owing to summation), we build an additional
tensor-factorization training pair by swapping a random action with
the last action from each finished game.
Algorithm discovery results
Discovery of matrix multiplication algorithms
We train a single AlphaTensor agent to find matrix multiplication algo-
rithms for matrix sizes n × m with m × p, where n, m, p ≤ 5. At the begin-
ning of each game, we sample uniformly a triplet (n, m, p) and train
AlphaTensor to decompose the tensor Tn m p, , . Although we consider
tensors of fixed size (Tn m p, , has size nm × mp × pn), the discovered algo-
rithms can be applied recursively to multiply matrices of arbitrary size.
We use AlphaTensor to find matrix multiplication algorithms over
different arithmetics—namely, modular arithmetic (that is, multiplying
matrices in the quotient ring 2Z ), and standard arithmetic (that is, mul-
tiplying matrices in R).
Figure 3 (left) shows the complexity (that is, rank) of the algo-
rithms discovered by AlphaTensor. AlphaTensor re-discovers the
best algorithms known for multiplying matrices (for example,
Strassen’s 2 and Laderman’s 15 algorithms). More importantly, AlphaT-
ensor improves over the best algorithms known for several matrix
sizes. In particular, AlphaTensor finds an algorithm for multiplying
4 × 4 matrices using 47 multiplications in 2Z , thereby outperforming
Strassen’s two-level algorithm 2 , which involves 7 2 = 49 multiplica-
tions. By applying this algorithm recursively, one obtains a practical
matrix multiplication algorithm in Z2 with complexity O N( )2.778 .
Moreover, AlphaTensor discovers efficient algorithms for multiply-
ing matrices in standard arithmetic; for example, AlphaTensor finds
a rank-76 decomposition of 4,5,5T , improving over the previous
state-of-the-art complexity of 80 multiplications. See Extended
Data Figs. 1 and 2 for examples.
AlphaTensor generates a large database of matrix multiplication
algorithms—up to thousands of algorithms for each size. We exploit
this rich space of algorithms by combining them recursively, with the
aim of decomposing larger matrix multiplication tensors. We refer
to refs. 25,26 and Appendix H in Supplementary Information for more
details. Using this approach, we improve over the state-of-the-art
results for more than 70 matrix multiplication tensors (with
n, m, p ≤ 12). See Fig. 3 (right) and Extended Data Table 1 for the results.
A crucial aspect of AlphaTensor is its ability to learn to transfer knowl-
edge between targets (despite providing no prior knowledge on their
relationship). By training one agent to decompose various tensors,
AlphaTensor shares learned strategies among these, thereby improv-
ing the overall performance (see Supplementary Information for
analysis). Finally, it is noted that AlphaTensor scales beyond current
computational approaches for decomposing tensors. For example, to
our knowledge, no previous approach was able to handle T4, which has
an action space 1010 times larger than T3. Our agent goes beyond this
limit, discovering decompositions matching or surpassing
state-of-the-art for large tensors such as 5T .
AlphaTensor rank
Modular Standard
Size
(n, m, p)
Best method
known
Best rank
known
(2, 2, 2) (Strassen, 1969)2 7 7 7
(3, 3, 3) (Laderman, 1976)15 23 23 23
49 47 49
98 96 98
(2, 2, 3) 11 11 11
(2, 2, 4) 14 14 14
(2, 2, 5) 18 18 18
(2, 3, 3) 15 15 15
(2, 3, 4) 20 20 20
(2, 3, 5) 25 25 25
(2, 4, 4) 26 26 26
(2, 4, 5) 33 33 33
(2, 5, 5) 40 40 40
(3, 3, 4) (Smirnov, 2013)18 29 29 29
(3, 3, 5) 36 36 36
(3, 4, 4) 38 38 38
(3, 4, 5) 48 47 47
(3, 5, 5) 58 58 58
(4, 4, 5) 64 63 63
(4, 5, 5) 80 76 76 200 400 600 800 1,000
Best rank known
0
5
10
15
20
25
30
Improvement in rank
(9, 9, 9)
(9, 9, 11)
(9, 10, 10)
(9, 11, 11)
(10, 10, 10)
(10, 11, 12)
(10, 12, 12)
(11, 11, 11)
(11, 12, 12)
(Strassen, 1969)2
(Hopcroft and Kerr, 1971)16
(Hopcroft and Kerr, 1971)16
(Hopcroft and Kerr, 1971)16
(Hopcroft and Kerr, 1971)16
(Hopcroft and Kerr, 1971)16
(Smirnov, 2013)18
(Smirnov, 2013)18
(Smirnov, 2013)18
(Sedoglavic and Smirnov, 2021)19
(3, 5, 5) + (2, 5, 5)
(2, 2, 2) ^ (2, 2, 2)
(2, 2, 2) + (2, 2, 1)
(2, 2, 2) + (2, 2, 2)
(2, 2, 2) + (2, 2, 3)
(4, 4, 2) + (4, 4, 3)
(2, 5, 5) ^ (2, 1, 1)
(4, 4, 4)
(5, 5, 5)
(Hopcroft and Kerr, 1971)16
Fig. 3 | Comparison between the complexity of previously known matrix
multiplication algorithms and the ones discovered by AlphaTensor. Left:
column (n, m, p) refers to the problem of multiplying n × m with m × p matrices.
The complexity is measured by the number of scalar multiplications (or
equivalently, the number of terms in the decomposition of the tensor). ‘Best
rank known’ refers to the best known upper bound on the tensor rank (before
this paper), whereas ‘AlphaTensor rank’ reports the rank upper bounds
obtained with our method, in modular arithmetic (Z2 ) and standard arithmetic.
In all cases, AlphaTensor discovers algorithms that match or improve over
known state of the art (improvements are shown in red). See Extended Data
Figs. 1 and 2 for examples of algorithms found with AlphaTensor. Right: results
(for arithmetic in R) of applying AlphaTensor-discovered algorithms on larger
tensors. Each red dot represents a tensor size, with a subset of them labelled.
See Extended Data Table 1 for the results in table form. State-of-the-art results
are obtained from the list in ref. 64 .
Nature | Vol 610 | 6 October 2022 | 51
Analysing the symmetries of matrix multiplication algorithms
From a mathematical standpoint, the diverse algorithms discovered
by AlphaTensor show that the space is richer than previously known.
For example, while the only known rank-49 factorization decomposing
T T T= ⊗4 2 2 before this paper conforms to the product structure (that
is, it uses the factorization of T2 twice, which we refer to as Strassen-
square 2 ), AlphaTensor finds more than 14,000 non-equivalent fac-
torizations (with standard arithmetic) that depart from this scheme,
and have different properties (such as matrix ranks and sparsity—see
Supplementary Information). By non-equivalent, we mean that it is
not possible to obtain one from another by applying a symmetry trans-
formation (such as permuting the factors). Such properties of matrix
multiplication tensors are of great interest, as these tensors represent
fundamental objects in algebraic complexity theory 3,5,7
. The study of
matrix multiplication symmetries can also provide insight into the
asymptotic complexity of matrix multiplication5
. By exploring this rich
space of algorithms, we believe that AlphaTensor will be useful for
generating results and guiding mathematical research. See Supple-
mentary Information for proofs and details on the symmetries of
factorizations.
Beyond standard matrix multiplication
Tensors can represent any bilinear operation, such as structured matrix
multiplication, polynomial multiplication or more custom bilinear
operations used in machine learning 27,28 . We demonstrate here a
use-case where AlphaTensor finds a state-of-the-art algorithm for
multiplying an n x n skew-symmetric matrix with a vector of length n.
Figure 4a shows the obtained decompositions for small instance sizes n.
We observe a pattern that we generalize to arbitrary n, and prove that
this yields a general algorithm for the skew-symmetric matrix-vector
product (Fig. 4b). This algorithm, which uses n n n( − 1)( + 2)/2 ~ 1
2
2
multiplications (where ∼ indicates asymptotic similarity), outperforms
the previously known algorithms using asymptotically n2 multiplica-
tions 29
, and is asymptotically optimal. See Supplementary Information
for a proof, and for another use-case showing AlphaTensor’s ability
to re-discover the Fourier basis (see also Extended Data Table 2). This
shows that AlphaTensor can be applied to custom bilinear operations,
and yield efficient algorithms leveraging the problem structure.
Rapid tailored algorithm discovery
We show a use-case where AlphaTensor finds practically efficient matrix
multiplication algorithms, tailored to specific hardware, with zero
prior hardware knowledge. To do so, we modify the reward of AlphaT-
ensor: we provide an additional reward at the terminal state (after the
agent found a correct algorithm) equal to the negative of the runtime
of the algorithm when benchmarked on the target hardware. That is,
we set r r λb′ = +t t t , where rt is the reward scheme described in ‘DRL for
algorithm discovery’, bt is the benchmarking reward (non-zero only at
the terminal state) and λ is a user-specified coefficient. Aside from the
different reward, the exact same formulation of TensorGame is used.
We train AlphaTensor to search for efficient algorithms to multiply
4 × 4 block matrices, and focus on square matrix multiplication of size
8,192 (each block is hence of size 2,048) to define the benchmarking
reward. AlphaTensor searches for the optimal way of combining the
16 square blocks of the input matrices on the considered hardware. We
do not apply the 4 × 4 algorithm recursively, to leverage the efficient
implementation of matrix multiplication on moderate-size matrices
(2,048 × 2,048 in this case). We study two hardware devices commonly
used in machine learning and scientific computing: an Nvidia V100
graphics processing unit (GPU) and a Google tensor processing unit
(TPU) v2. The factorization obtained by AlphaTensor is transformed
into JAX30 code, which is compiled ( just in time) before benchmarking.
Figure 5a,b shows the efficiency of the AlphaTensor-discovered
algorithms on the GPU and the TPU, respectively. AlphaTensor dis-
covers algorithms that outperform the Strassen-square algorithm,
which is a fast algorithm for large square matrices 31,32 . Although
the discovered algorithm has the same theoretical complexity as
Strassen-square, it outperforms it in practice, as it is optimized for
the considered hardware. Interestingly, AlphaTensor finds algorithms
n = 3
Extrapolationa
( n – 1 )( n + 2 )
2 multiplications.
(2)
(3) w ij = a ij ( bj – bi ) Computing the first ( n – 2 )( n + 1 )/2 intermediate products
(4)
(5) q i = b i
n
j=1 a ji
(7)
(8)
(9)
Input: n × n skew-symmetric matrix A, vector b.
Output: The resulting vector c = Ab computed in
(1) for i = 1, . . . , n − 2 do
for j = i + 1, . . . , n do
(4) for i = 1, . . . , n do
Σ
(6) for i = 1, . . . , n − 2 do
Computing the final n intermediate products
ci = i–1
j=1 w ji +Σ n
j=i+1wij – qiΣ
cn–1 = – n–2
i=1Σ w ij –
n–2
j=i+1Σ wjn +
n–2
j=1Σ q i
n
i=1,i≠n–1Σ
cn = – n–1
i=1Σ wij +
n
j=i+1Σ q i
n–1
i=1Σ
b W V U
n = 4 n = 5 n = 6 n = 10
Fig. 4 | Algorithm discovery beyond standard matrix multiplication.
a, Decompositions found by AlphaTensor for the tensors of size n n× ×
n n( − 1)
2
(with n = 3, 4, 5, 6) representing the skew-symmetric matrix-vector multiplication.
The red pixels denote 1, the blue pixels denote −1 and the white pixels denote 0.
Extrapolation to n = 10 is shown in the rightmost figure. b, Skew-symmetric
matrix-by-vector multiplication algorithm, obtained from the examples solved
by AlphaTensor. The w ij and q i terms in steps 3 and 5 correspond to the m r terms
in Algorithm 1. It is noted that steps 6–9 do not involve any multiplications.
52 | Nature | Vol 610 | 6 October 2022Article
with a larger number of additions compared with Strassen-square (or
equivalently, denser decompositions), but the discovered algorithms
generate individual operations that can be efficiently fused by the
specific XLA 33 grouping procedure and thus are more tailored towards
the compiler stack we use. The algorithms found by AlphaTensor also
provide gains on matrix sizes larger than what they were optimized
for. Finally, Fig. 5c shows the importance of tailoring to particular
hardware, as algorithms optimized for one hardware do not perform
as well on other hardware.
Discussion
Trained from scratch, AlphaTensor discovers matrix multiplication
algorithms that are more efficient than existing human and
computer-designed algorithms. Despite improving over known
algorithms, we note that a limitation of AlphaTensor is the need
to pre-define a set of potential factor entries F, which discretizes
the search space but can possibly lead to missing out on efficient
algorithms. An interesting direction for future research is to adapt
AlphaTensor to search for F. One important strength of AlphaTensor
is its flexibility to support complex stochastic and non-differentiable
rewards (from the tensor rank to practical efficiency on specific hard-
ware), in addition to finding algorithms for custom operations in a
wide variety of spaces (such as finite fields). We believe this will spur
applications of AlphaTensor towards designing algorithms that opti-
mize metrics that we did not consider here, such as numerical stability
or energy usage.
The discovery of matrix multiplication algorithms has far-reaching
implications, as matrix multiplication sits at the core of many com-
putational tasks, such as matrix inversion, computing the determi-
nant and solving linear systems, to name a few 7
. We also note that our
methodology can be extended to tackle related primitive mathemati-
cal problems, such as computing other notions of rank (for example,
border rank—see Supplementary Information), and NP-hard matrix
factorization problems (for example, non-negative factorization). By
tackling a core NP-hard computational problem in mathematics using
DRL—the computation of tensor ranks—AlphaTensor demonstrates the
viability of DRL in addressing difficult mathematical problems, and
potentially assisting mathematicians in discoveries.
Online content
Any methods, additional references, Nature Research reporting summa-
ries, source data, extended data, supplementary information, acknowl-
edgements, peer review information; details of author contributions
and competing interests; and statements of data and code availability
are available at https://doi.org/10.1038/s41586-022-05172-4.
1. Silver, D. et al. A general reinforcement learning algorithm that masters chess, shogi, and
Go through self-play. Science 362, 1140–1144 (2018).
2. Strassen, V. Gaussian elimination is not optimal. Numer. Math. 13, 354–356 (1969).
3. Bürgisser, P., Clausen, M. & Shokrollahi, A. Algebraic Complexity Theory Vol. 315 (Springer
Science & Business Media, 2013).
4. Bläser, M. Fast matrix multiplication. Theory Comput. 5, 1–60 (2013).
5. Landsberg, J. M. Geometry and Complexity Theory 169 (Cambridge Univ. Press, 2017).
6. Pan, V. Y. Fast feasible and unfeasible matrix multiplication. Preprint at https://arxiv.org/
abs/1804.04102 (2018).
7. Lim, L.-H. Tensors in computations. Acta Numer. 30, 555–764 (2021).
8. Schönhage, A. Partial and total matrix multiplication. SIAM J. Comput. 10, 434–455 (1981).
9. Coppersmith, D. & Winograd, S. Matrix multiplication via arithmetic progressions. In ACM
Symposium on Theory of Computing 1–6 (ACM, 1987).
10. Strassen, V. The asymptotic spectrum of tensors and the exponent of matrix
multiplication. In 27th Annual Symposium on Foundations of Computer Science 49–54
(IEEE, 1986).
11. Le Gall, F. Powers of tensors and fast matrix multiplication. In International Symposium on
Symbolic and Algebraic Computation 296–303 (ACM, 2014).
12. Alman, J. & Williams, V. V. A refined laser method and faster matrix multiplication. In
ACM-SIAM Symposium on Discrete Algorithms 522–539 (SIAM, 2021).
13. Gauss, C. F. Theoria Motus Corporum Coelestium in Sectionibus Conicis Solum
Ambientium (Perthes and Besser, 1809).
14. Hillar, C. J. & Lim, L.-H. Most tensor problems are NP-hard. J. ACM 60, 1–39 (2013).
15. Laderman, J. D. A noncommutative algorithm for multiplying 3 × 3 matrices using 23
multiplications. Bull. Am. Math. Soc. 82, 126–128 (1976).
16. Hopcroft, J. E. & Kerr, L. R. On minimizing the number of multiplications necessary for
matrix multiplication. SIAM J. Appl. Math. 20, 30–36 (1971).
17. Vervliet, N., Debals, O., Sorber, L., Van Barel, M. & De Lathauwer, L. Tensorlab 3.0 (2016);
https://www.tensorlab.net/
18. Smirnov, A. V. The bilinear complexity and practical algorithms for matrix multiplication.
Comput. Math. Math. Phys. 53, 1781–1795 (2013).
19. Sedoglavic, A. & Smirnov, A. V. The tensor rank of 5x5 matrices multiplication is bounded
by 98 and its border rank by 89. In Proc. 2021 on International Symposium on Symbolic
and Algebraic Computation 345–351 (ACM, 2021).
20. Heule, M. J., Kauers, M. & Seidl, M. New ways to multiply 3 × 3-matrices. J. Symb. Comput.
104, 899–916 (2021).
21. Hubert, T. et al. Learning and planning in complex action spaces. In International
Conference on Machine Learning 4476–4486 (PMLR, 2021).
22. Zhang, W. & Dietterich, T. G. A reinforcement learning approach to job-shop scheduling.
In International Joint Conferences on Artificial Intelligence Vol. 95, 1114–1120
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
TPU
GPU
Benchmark device
10.3%
2.8%
4.4%
8.5%
Optimized for TPU
Optimized for GPU
Speed-up on Nvidia V100 GPU Speed-up on TPU v2 Speed-up of tailored agorithms
on both devices
a b c
Strassen-square
AlphaTensor
Strassen-square
Fig. 5 | Speed-ups of the AlphaTensor-discovered algorithm. a,b, Speed-ups
(%) of the AlphaTensor-discovered algorithms tailored for a GPU (a) and a TPU
(b), optimized for a matrix multiplication of size 8,192 × 8,192. Speed-ups are
measured relative to standard (for example, cuBLAS for the GPU) matrix
multiplication on the same hardware. Speed-ups are reported for various
matrix sizes (despite optimizing the algorithm only on one matrix size). We also
report the speed-up of the Strassen-square algorithm. The median speed-up is
reported over 200 runs. The standard deviation over runs is <0.4 percentage
points (see Supplementary Information for more details). c, Speed-up of both
algorithms (tailored to a GPU and a TPU) benchmarked on both devices.
Nature | Vol 610 | 6 October 2022 | 53
23. Vaswani, A. Attention is all you need. In International Conference on Neural Information
Processing Systems Vol 30, 5998–6008 (Curran Associates, 2017).
24. Ho, J., Kalchbrenner, N., Weissenborn, D. & Salimans, T. Axial attention in
multidimensional transformers. Preprint at https://arxiv.org/abs/1912.12180 (2019).
25. Drevet, C.-É., Islam, M. N. & Schost, É. Optimization techniques for small matrix
multiplication. Theor. Comput. Sci. 412, 2219–2236 (2011).
26. Sedoglavic, A. A non-commutative algorithm for multiplying (7 × 7) matrices using 250
multiplications. Preprint at https://arxiv.org/abs/1712.07935 (2017).
27. Battaglia, P. W. et al. Relational inductive biases, deep learning, and graph networks.
Preprint at https://arxiv.org/abs/1806.01261 (2018).
28. Balog, M., van Merriënboer, B., Moitra, S., Li, Y. & Tarlow, D. Fast training of sparse graph
neural networks on dense hardware. Preprint at https://arxiv.org/abs/1906.11786 (2019).
29. Ye, K. & Lim, L.-H. Fast structured matrix computations: tensor rank and Cohn–Umans
method. Found. Comput. Math. 18, 45–95 (2018).
30. Bradbury, J. et al. JAX: composable transformations of Python+NumPy programs. GitHub
http://github.com/google/jax (2018).
31. Benson, A. R. & Ballard, G. A framework for practical parallel fast matrix multiplication.
ACM SIGPLAN Not. 50, 42–53 (2015).
32. Huang, J., Smith, T. M., Henry, G. M. & Van De Geijn, R. A. Strassen’s algorithm reloaded.
In International Conference for High Performance Computing, Networking, Storage and
Analysis 690–701 (IEEE, 2016).
33. Abadi, M. et al. Tensorflow: a system for large-scale machine learning. In USENIX
Symposium On Operating Systems Design And Implementation 265–283 (USENIX,
2016).
Publisher’s note Springer Nature remains neutral with regard to jurisdictional claims in
published maps and institutional affiliations.
Open Access This article is licensed under a Creative Commons Attribution
4.0 International License, which permits use, sharing, adaptation, distribution
and reproduction in any medium or format, as long as you give appropriate
credit to the original author(s) and the source, provide a link to the Creative Commons license,
and indicate if changes were made. The images or other third party material in this article are
included in the article’s Creative Commons license, unless indicated otherwise in a credit line
to the material. If material is not included in the article’s Creative Commons license and your
intended use is not permitted by statutory regulation or exceeds the permitted use, you will
need to obtain permission directly from the copyright holder. To view a copy of this license,
visit http://creativecommons.org/licenses/by/4.0/.
© The Author(s) 2022