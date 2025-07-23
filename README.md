# dataviz
Codes for data visualizations posted on Youtube.

Youtube: https://www.youtube.com/sundellviz

Reddit: https://www.reddit.com/user/desfirsit

## Empire simulation:
The main file that produces the Shiny application is shiny_empires2.R

The assests used are produced by the file shiny_premake.R.

## Evolution
1. Evolution of shapes: https://sundellviz.github.io/dataviz/...
100 random "creatures" randomly bump into one another, and if a blue claw touches a red heart the creature is copied.

2. Evolution of attributes: https://sundellviz.github.io/dataviz/...
Round prey seek out white plants, and triangular predators seek out prey. When they eat, they copy with mutation. After a while, more beneficial attributes (such as speed) will become more common.

3. Evolution of behavior: https://sundellviz.github.io/dataviz/...
Blue prey animals and red predators are born with random neural networks. When they eat they copy with mutation. After a while, those with behaviors that encourage reproduction will become more common - but watch so that they don't die of starvation.

4. Training to run: https://sundellviz.github.io/dataviz/...
This is actually not evolution so much as breeding. Stick figures try to move to the right. Each run consists of several heats and the result is based on the average of the heats. After each full run the best runner's neural network is copied with mutation to the next generation of runners. This is more breeding than evolution.

