# rangoli
Some code that I wrote to create some Rangoli-like patterns. 

I haven't bothered writing comments, etc. so apologiesfor that, but if you get R, it should be self-explanatory. 

The main method is called "newpattern". First argument is "sides" where you input what sided polygon you want to start off with. The code creates a regular polygon of that size, divides it into triangles and creates these wigglies inside each of the triangles. 

I would recommend that you put "relative" as TRUE, since that means that the length of the lines will decrease as you go inward. The argument "increment" tells what proportion of the distance to the next line needs to be covered in the next segment, and so on. 

Feel free to play around with this! 
