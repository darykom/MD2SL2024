# Errata corrige for Exercise 1

**Approximately 1/125 of all births are fraternal twins and 1/300 of births are identical twins. Elvis Presley had a twin brother (who died at birth). What is the probability that Elvis was an identical twin?**

Unfortunately my solution for Exercise 1 was wrong. The right approach is described below.

We define:
* $P(\text{fraternal twins}) = 1/125$
* $P(\text{identical twins}) = 1/300$
* $P(\text{boy}) = P(\text{girl}) = 1/2$

The probability that Elvis was an identical twin, given the fact that he had a twin brother, can be derive using Bayes Theorem: $P(A|B) = \frac{P(B|A) P(A)}{P(B)}$.  
Thus
$$
P(\text{Identical} \mid \text{Twin Brother}) = \frac{P(\text{Twin Brother} \mid \text{Identical}) \cdot P(\text{Identical})}{P(\text{Twin Brother})}
$$

The probability of being twin brothers (so, both boys) is given by the sum of the probabilities of two events, being _identical twins and twin brother_ and being _fraternal twins and twin brother_.  
Indeed, being identical twins and being fraternal twins are two mutually exclusive events: two twin brothers can only be or fraternal or identical twins; so it holds the so called _Law of total Probability_.

$$
P(\text{Identical} \mid \text{Twin Brother}) = \frac{P(\text{Twin Brother} \mid \text{Identical}) \cdot P(\text{Identical})}{P(\text{Twin Brother})}
$$
$$
P(\text{Identical} \cap \text{Twin Brother}) = P(\text{Twin Brother} \mid \text{Identical}) \cdot P(\text{Identical}) = \frac{1}{2} \cdot \frac{1}{300} = \frac{1}{600}
$$

$$
P(\text{Fraternal} \cap \text{Twin Brother}) = P(\text{Twin Brother} \mid \text{Fraternal}) \cdot P(\text{Fraternal}) = \frac{1}{4} \cdot \frac{1}{125} = \frac{1}{500}
$$
$$
P(\text{Twin Brother}) = P(\text{Identical} \cap \text{Twin Brother}) + P(\text{Fraternal} \cap \text{Twin Brother}) =  \frac{1}{600} + \frac{1}{500} = \frac{11}{3000}
$$

Finally
$$
P(\text{Identical} \mid \text{Twin Brother}) = \frac{\frac{1}{2} \cdot \frac{1}{300}}{\frac{11}{3000}} = \frac{1}{600} \cdot \frac{3000}{11} = \frac{5}{11}
$$