(ns recircules.fracticious)

"This namespace is for the definition of fractional pattern generation systems.

These systems can be used in conjunction with density metrics and density targets
to generate patterns with well distributed features

Fractional systems are typically generated on the loop level. 
It is an iterative procedure of looking at each existing point and comparing 
its density to the integral of a target density function from the point to the next
if there is a deficit the point will introduce one more intervening the period to the 
next point. 

The process is repeated until adequte saturation level is attained 
or maximum iteration level is reached



It would be possible however to also do occurance level fracturing. 
This would involve scheduling forward to the next point but,
 if there is a deficit to schedule INSTEAD at <half>way between the present and the next point 

problematically this would result in getting off a rational split of the complete pattern

so the density would resolve to a target seperation.
 This seperation would be approached by binary search.

target 0.6666  0 -> 0.5 -> 0.75 -> 0.625 
 
Assumption: events can only occur at points in a pattern or at a hypthetical point 
partway between two adjacent points of the pattern or other hypthetical points,
provided that the hypthetical points are only available within the scope of
 the last event scheduling the 

"

