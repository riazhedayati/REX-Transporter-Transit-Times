06.2018

Business Problem:
Transporters are not optimally matched with patients needing to be transported, leading to bottlenecks in patient throughput. 

Project Goal:
Create an optimization model to optimally match supply and demand of transporters. As an input to the optimization, we need time estimates between every possible origin-destination department pairing.

Results:
Used historical mean of transit times for all origin-destination department pairings with at least 5 recorded trips. Used a shortest path algorithm to estimate transit times for department pairings without enough historical data. 

Challenges/Learnings/Takeaways:
Current data uses historical trips including all equipment and patient time. As an additional input to the optimization we must collect data on transit time excluding patient and equipment time, as no historical data exists that captures that information.
