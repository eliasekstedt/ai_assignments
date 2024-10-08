# save between steps
* computed probabilities for each node (a matrix)
* searched nodes (according to instructions)

# node prob computation
* current node prob given readings X SUM_over_neighbor(previous_neighbor_probs) X 1/nr_neighbors aka, transition_prob

######
######
######


runWheresCroc(makeMoves, doPlot = T, showCroc = F, pause = 1,
  verbose = T, returnMem = F, mem = NA)


# makeMoves - Your function that takes five arguments
1) A list of information for the move. This has two fields
    * The first is a vector of numbers called 'moves', where you will enter the moves you want to make. You should
    enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the numbers of a neighboring or current waterhole or '0' which means you will search your current waterhole for Croc.
    * The second field is a list called 'mem' that you can use to store information you want to remember from turn to turn.
2) A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location.
3) A vector giving the positions of the two tourists (elements 1 and 2) and yourself (element 3). If a tourist
has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist
was eaten by Croc in a previous turn, then the position will be NA.
4) a two column matrix giving the edges paths between waterholes (edges) present (the numbers are from and to numbers for the waterholes). All edges can be crossed both ways, so are only given once.
5) a list of three matrices giving the mean and standard deviation of readings for salinity, phosphate and nitrogen respectively at each waterhole. Your function should return the first argument passed with an updated moves vector
and any changes to the 'mem' field you wish to access later on.

# doPlot - A Boolean stating if you want the gameboard to be plotted each turn

# showCroc - A Boolean value specifying whether you want Croc to be shown on the gameboard.
Note that you are not permitted to use this visual information when you are scored.

# pause - The pause period between moves
Designed to give time to plot game.

# verbose
Set to FALSE to stop any print output

# returnMem
Should the info$mem field be returned? If so, the output is a list consisting of the move field, giving the number of moves in the game, and the mem field consisting of the mem object

# mem
If you returned a mem object from a previous run, it can be passed here. It's status will be set to 1. Otherwise a new mem list will be created with status set to 0. The idea is to speed up multiple runs, such as the evaluation run of 500 games, by avoiding redoing expensive initial setups of the transition matrix and routing informing.

\value{
A string describing the outcome of the game.
}
\description{
Runs the Where's Croc game. In this game, you are a ranger in an Australian national park.
This park consists of a number of waterholes, some of which are connected to each other.
There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record
the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also
fitted with a sensor that records his position, but that has broken.
Your task is to find Croc using the available information. To aid in this you have information
about the probability distributions for different salinity, phosphate and nitrogen levels in
different waterholes.
There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
moving to one of the neighboring waterholes from where they are or staying still. All moves
are equally likely.
If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
the waterhole you are on when Croc is there, you have found Croc and win the game.
Your score is the number of turns it takes to find Croc.
To play manually pass manualWC
as the makeMoves function and enter the appropriate numbers to make moves.
Note that the croc will move randomly, with a uniform distribution over moving to any adjancent waterholes
or staying still.
}
