/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* -*-C++-*- */

/**
*** This file decides how to block for the memory hierarcy (e.g. cache).
*** Basically, we determine how much data is brought into the cache
*** within a blocked nest.  That helps us choose sensible tile sizes
*** to maximize the effectiveness of blocking.
***
*** Here is a list of topics covered in this extensive list of notes:
***
*** (1.0) THE CACHE MODEL 
*** (1.1)   The Alternatives 
*** (1.2)   The Role of Footprints and Blocking Factors
*** (1.3)   Relating the Footprint to Cache Requirements
***
*** (2.0) COMPUTING FOOTPRINTS  
*** (2.1)   Single Reference, Single Element Cache Line Case
*** (2.2)   Multiple Word Cache Lines
*** (2.3)   Modifying X for Multiple References
*** (2.4)   Modifications for Large Cache Lines
*** (2.5)   When the Innermost Loop Doesn't Need to Be Blocked
*** (2.6)   Accounting for Multiple References
*** (2.7)   Implementation of Reference Groups in this Code
*** (2.8)   Computing "d" in the Requirement Formula
***
*** (3.0) THE BLOCKING MODEL 
*** (3.1)   The Square and Rectangular Blocking Models 
*** (3.2)   Computing the Loop Overhead
*** (3.3)   Solving for Block Sizes by Recursive Descent
*** (3.4)   Testing Different Loop Orders
*** (3.5)   When the Footprint Fits in the Cache
*** (3.6)   Blocking for Multiple Levels of Cache
*** (3.7)   Blocking for L1, then L2
***
*** (4.0) ADDITIONAL ASPECTS OF THE MODEL
*** (4.1)   Overlapping Memory Misses with Machine Cycles
*** (4.2)   Clean and Dirty Miss Penalties
*** (4.3)   Hiding Cache Misses in Computation Time
*** (4.4)   The Power of Two Hack Code:  Hitting the Same Cache Line
*** (4.5)   How Non Constant Loops Effect the Footprint
*** (4.6)   Modeling the TLB
***
*** (5.0) SOME SUGGESTED IMPROVEMENTS
*** (5.1)   Reusing the Footprints and Footprint Code
*** (5.2)   Dealing with Unusual References                 
***
*** (6.0) READING THE CACHE DEBUGGING TRACE
*** (6.1)   The STDOUT -Wb,-tt31:0x8000 Trace
***
***
***
*** 		GENERAL NOTES ON THE SGI CACHE MODEL  
***
*** (1.0) THE CACHE MODEL 
***
*** (1.1) The Alternatives 
***
*** The reuse model is based upon uniformly generated sets and vector
*** spaces.  It was a tough call, whether to use this, or a more ad-hoc
*** model (which would have had more special cases but would have been
*** easier because we wouldn't have needed to implement vector spaces,
*** which are a pain).  Vector spaces would have lost out to the ad-hoc
*** approach, except that I wanted the model to be robust and cheap
*** in the face of skewing.  Skewing won't happen in this first
*** implementation,  but it's nice to not have to throw everything away
*** for the next implementation, anyway.  Also, the prefetching algorithm
*** probably functions best with prefetching also, so might as well
*** bite the bullet.
***
*** We could have based our approach on some sort of counting method
*** that computes very close to the actual size of the footprint, as
*** I believe Jalby and Gannon propose.  However, the approach in this code
*** is significantly more straightforward than their approach, and should
*** give very good answers almost all the time.
***
*** NOTE: Rohit's cache model used in the prefetcher incorporates the best
*** aspects of this model combined with nice improvements, including
*** better ability to model multiple levels of cache.  Also, considering
*** the similarity of blocking and prefetching, it might be a significant
*** cleanup (and speedup) to combine the two.  But not necessarily -- they
*** might just be too different.  Not really sure, but this is good for now.
*** The model implemented here assumes that we know exactly which loops
*** we are blocking.  We block our loops, and so they (or strips of them)
*** fit into the cache and nothing else does.  This is much simpler and
*** faster than the prefetching model.  It's wrong in that sometimes
*** loops further out will also fit in the cache because of small loop
*** bounds.  We don't consider loops outside the SNL yet, but when we
*** do, will Rohit's model help?
***
***
*** (1.2) The Role of Footprints and Blocking Factors 
***
*** In the following discussion, array references with square brackets, 
*** e.g. a[i,j], are arrays stored in row-major order (C, Ada, Whirl), 
*** while those in round brackets, e.g. a(i,j), are stored in column- 
*** major order (Fortran). 
***
*** Suppose we have the nest
***
***	for i1 = 1, N1	
***	 for i2 = (blocked size N2, a.k.a. B2)
***	  for i3 = (blocked size N3, a.k.a. B3)
***	   ...
*** If N1 is not constant, it may be fine to assume 100, or some similar
*** constant value.
***
*** Let F2 be the quantity of bytes touched by all iterations of
*** i2, i3, ... and let F1 be the quantity of data touched in all
*** iterations of i1, i2, i3, ... .  This blocking is non-sensical
*** unless the reuse in adjacent iterations of i1 is taken advantage
*** of, and that's only possible if in loops further in, reuse is taken
*** advantage of to a very large extent.  Thus F2 is really
*** a reasonable estimate of the data the cache requires to execute
*** i2, i3, ... , because there should be very few misses of reused
*** data at this level.  Misses of reused data in the i1 loop may cause F1
*** to understimate the bandwidth needed to execute all iterations
*** of i1, however.  (This assumption that reuse occurs from i2 in, and
*** outside i1, and maybe at i1, is the assumption that Rohit's model
*** can improve upon.)
***
*** Note the subtlety:  Although we have not necessarily chosen any Bs yet,
*** we know that we will choose them so that reuse in i2, i3, etc are
*** achieved, so therefore we can assume reuse in i2, i3, etc are achieved.
*** If we chose a B such that this reuse did not occur,
*** then this tiling is silly, and better code can be generated with a
*** different tiling, e.g. not stripping the i2 loop.  Also, it is possible
*** that the i1 loop has so few iterations that reuse can occur outside it,
*** or that one of the inner loops has so few iterations that the B that
*** would have been chosen would be too large.
***
*** Small loop bounds are accounted for, when they are in the SNL.  When
*** a loop bound is small, the model realizes that the blocksize
*** need not be larger than that amount, and if the blocksize is that amount,
*** it doesn't block it.  If the iteration count is very small, the model
*** will even realize that less of the cache is used and use higher
*** tripcounts for the other loops.  TODO OK: Note
*** that if the inner loop has small loop bounds, and loops outside it
*** cannot be tiled with this inner loop, then there is an innermost SNL of
*** depth 1 and we are done.  But better would be to transform outer SNL's
*** for locality by seeing if all loops inside have small loop bounds, and
*** if so, tiling anyway.  For a machine with a big cache, this could be very
*** important, since the innermost's "small bounds" wouldn't have to be too
*** small.  I think this is very doable, and also very important.  E.g.
*** Householder QR can have the outer two loops blocked but the inner loop
*** cannot be (dependence (+,+,*)).  There is a large range of problem sizes
*** for which run-time performance improvement will be significant, although
*** if N is too large then just the inner loop will overrun the cache.
*** B will end up being a function of N: the larger N, the smaller B.
*** 
*** 
*** (1.3) Relating the Footprint to Cache Requirements 
***
*** We are concerned about reusing data in i1 while still having space in
*** the cache.  The quantity of new data brought in by each iteration of i1
*** is roughly ND = (F1-F2)/(N1-1). 
***
*** Note that in the following example, if we suppose a cache line has 8 
*** words,  
***
***	do i = 1, N
***	 do j = 1, B
***	  a(i)
***
*** the inner loop must bring in at least 8 words of data and the outer loop 
*** will brings in one new word per iteration, reusing the other 7, because 
*** it's reusing the cache line, which counts.  
***
*** There is a maximum distance d in the outermost loop for which we are i
*** required to keep data in memory.  In the above example, d = 7, since as 
*** long as the data used in iteration i is still available in iteration i+7, 
*** reuse will be successful.  Later we discuss how to compute d.
***
*** Suppose the cache holds C elements.  Then the cache needs to hold
*** F2+ND*d elements to take advantage of all the reuse. 
***
*** Let p be between 0 and 1 (typical value: 0.1 or 0.15) -- p is part
*** of the machine description, along with C, computed from cache
*** characteristics. The quantity p*C is the effective cache size.  The 
*** factor p is less than 1 when the cache is not fully associative, 
*** which is currently the case for all caches in SGI machines today.  
***
*** If F2+ND*d <= p*C, then almost all the potential data
*** is reused.  Otherwise, lots of the potentially reused data may be
*** wasted.  See the #defines for more information on this matter.
*** Given the reuse, new data, footprints, etc, it is easy to come up
*** with an equation that tells us the number of misses per iteration,
*** and so the number of cycles per iteration.
***
***
*** (2.0) COMPUTING FOOTPRINTS 
***
*** At this point, we have discussed everything about blocking for one
*** cache level except for computing F1, F2 and d.
***
*** Let's discuss computing F1 and F2; computing d we will take up
*** later.  We use the same approach to compute F1 and F2, since they are
*** really the same problem, with F2 just having one less loop in the nest.
*** So all we have to do is show how to compute F1.  (TODO OK: by
*** computing F1 and F2 independently, one could argue, correctly, that
*** we have not taken advantage of the similarity of the problems, and
*** so we do twice as much work as necessary.  True, but it's apparently
*** not the bottleneck.)  So now we show how to compute F1. The discussion 
*** will be in terms of elements, but the code, to allow for different 
*** element sizes (floats and doubles, for example) does all its computa-
*** tion using bytes rather than elements.
***
***
*** (2.1) Single Reference, Single Element Cache Line Case   
***
*** We begin with the case of a loop nest with a simple reference of the 
*** form:
***	    _
***	for i
***	      _     _   _
***	 x [ Hi + A.k + c ]
***                                                      _     _
*** This means a matrix H multiplies the index variables i.  A.k is a matrix 
*** product, where k are variables that are not index variables, e.g. N.
*** Finally, c is a vector of integer constants.  Uniformly generated
*** references have the same H and A -- they only differ in by an integer
*** constant in each dimension.  I use the subscript _s to indicate that
*** the stride one dimension is ignored.  E.g. H_s is H with the last row
*** elided.
***
*** Begin by assuming the cache line size is one.  If ker H is null, then
*** the number of elements used are Prod(Ni).  In general, suppose O is
*** the orthogonal subspace of ker H, roughly written O = I - ker H where I
*** is the iteration space.  "Beautify" O so that the basis vectors are
*** elementary basis vectors of I insofar as possible.  Suppose the basis
*** vectors for O are o_i where o_i = sum c_ik e_k.  
***
*** Then the number of items touched by reference x are: 
***
***	   PROD         SUM        P * A_k   
***     OKerH basis    basis      --------   
***      vector i    component     |c_ik|  
***		     c_ik != 0 
***
***     where: P =  PROD |(cij)|  and A_k = N_k (the trip count of loop k) 
***                cij != 0		        (at least for now) 
*** 
***  There are some notes and exceptions: 
***
***  (1) This formula holds as long as all |c_ik| <= 16.  If not, we pretend
***      c_ik = 0, and add another basis vector j with c_jk = 1 and c_jl = 0
***      for l != k.  This has the effect of treating a(i+23*j) as if it were
***	 a(i,j).  In other words, a(i+23*j) looks like a linearized reference.
***  (2) The value of 16 is chosen arbitrarily.  Michael wanted it big enough
***	 so that if we unroll by 16, this work as if you would expect. 
***  (3) The original formula was: 
***
***	   PROD             SUM        P * A_k   
***     OKerH basis  ((    basis      --------  ) - (P-1))  
***      vector i        component     |c_ik|  
***		         c_ik != 0 
***
*** 	  Right now, we don't subtract this term, because we were afraid 
***	  the result would go negative.  But Michael believed that it  
***	  might be possible to include this term back in.  (It will take 
***	  some more time to figure out what it signifies.) 
***
***	Let's look at an example: 
***
***	for i
***	 for j
***	  x[j] = y[i+j]
***
***	Suppose the vectors look like [i j].  Then for reference x, 
***	as basis for ker H is [1 0], and a basis for OKerH is [0 1].     
***	[c_00 c_01] = [0 1].  P = |c_01| = 1.  The result is: 
***	1 * N_j / |1| = N_j.  For reference y, a basis for is [1 1]. 
***     so that [c_00 c_01] = [1 1], P = |c_00| * |c_01| = 1, and 
***	the result is: 1 * N_i / |1| + 1 * N_j / |1| = N_i + N_j. 
***
*** Note that OkerH means the orthogonal space to the kernel of H.  So it's 
*** the basis vectors of the space without temporal locality.  If the cache
*** line size is larger than the element size, the above formula must be 
*** multipied by the cls/eltsz, where cls is the cache line size, and  
*** eltsz is the element size.  
***
***  
*** (2.2) Multiple Word Cache Lines
***
*** Ok, what happens when a cache line holds more than one word?  Let the
*** cache line size be cls elements.  We want to find which loop is the one
*** reusing the cache line but not accounted for already in the number of
*** elements touched.  First of all, we check that dim(ker H) < dim(ker Hs),
*** where Hs is H with the stride one row removed.  If the inequality is
*** true, there is cache line use that is not accounted for in the temporal
*** reuse.  If the inequality does not hold, then use the formula above as
*** before. 
***
*** But if it does hold, then suppose the reference whose stride one
*** component looks like k1*i1+k2*i2+... .  Let L = {j: k_j != 0 and
*** j doesn't appear alone in another index besides possibly the stride one
*** index.}  L cannot be empty if dim(ker H) < dim(ker Hs).  (This is easy 
*** to prove by assuming L is empty and concluding ker H = ker Hs).  So 
*** choose j such that |k_j| is the minimum of all |k_l| where l in L.  
*** This j loop is the one we will call the "stride one loop".  This is a 
*** heuristic, but it will work in almost all cases.  (Again, Rohit's model 
*** does a better job.)
***
*** [What is the reasoning behind this?  Consider the case a[k,2*i+j+k].
*** Assuming Whirl's row major order, the stride one component is 2*i+j+k,
*** which indexes three loops, i, j, and k.  We're looking to choose the
*** loop "l" for which advancing the index "l" gives the smallest incre-
*** ment, which is hopefully less than a cache line.  The loop "k" is  
*** generally a bad choice, because advancing k will advance us D_0+1 
*** elements, where D_0 is the length in elements of the first dimension
*** of "a".  Most of the time D_0 will be substantially larger than 1. 
*** This leaves us with a choice of i or j.  Since advancing i moves us 
*** two elements, while advancing j moves us 1 element, we will select 
*** j as the stride 1 loop.  RJC] 
***
*** Now define X by: 
***
***     X = min(|k_j|,cls).  
***
*** Redefine A_k as: 
***
***	A_k = N_k,                   for k, a non-"stride one loop"
***	      (N_j-1)*X + cls        for j, the "stride one loop"
***
*** Using this in our original formula: 
***
***	   PROD         SUM        P * A_k   
***     OKerH basis    basis      --------   
***      vector i    component     |c_ik|  
***		     c_ik != 0 
***
***     where: P =  PROD |(cij)|  and A_k as above. 
***                cij != 0		 
***
*** yields our general formula for a single reference.
***
*** However, we should note one additional point, when there is no stride
*** one loop, we need to multiply by the cls in elements to get the entire
*** footprint.
***
*** (2.3) Modifying X for Multiple References 
***
*** However, there are some corrections we'd like to make to this formula.
*** Consider a[3*i] and a[3*i+1] with a cls of 1.  In each case, we get X = 
*** min(3,1) = 1, yielding a sum of 2*N_i for both references.  But this  
*** overestimates the data pulled into the cache, because the two references
*** share cache lines.  Hence, we use a modified formula for X in this case:
***
***	X = min(|k_j|, cls+spread)
***
*** where "spread" is the absolute value of the difference between the 
*** largest and smallest constant terms (signs included).  In this example,
*** spread = 1, if the two references were a[6*i+6] and a[6*i-5], the 
*** spread would be 11.  This X is used once for the entire group of 
*** references. 
***
*** [This formula will hold as long as the spread is small compared to 
*** the cache line size.  To illustrate it's validity, for strides greater
*** than the cache line size, assume that we have two references which are 
*** a distance S apart where S is smaller than the cache line size cls.  
*** The probability that the first reference and second reference are in 
*** the same cache line is (cls-S)/cls.  So the expected amount of data
*** pulled in is [1*(cls-S)/cls+2*S/cls]*cls*N_j = (1-S/cls)*cls*N_k, 
*** yielding a value of X = (1-S/cls)*cls = cls + S. RJC] 
*** 
*** TODO: This formula is completely wrong when the spread is a multiple
*** of the cache line size.  To see this, consider the case of two refer-
*** ences a[3*i] and a[3*i+3], with a cache line size of 1.  Here our 
*** formula gives X = min(3,3+3) = 6, when in reality we want something 
*** closer to 3. Michael notes:   
***
*** When these things go wrong, they matter.  (Perhaps change the data
*** structure for RG_NODE, so that rather than just a max and min stride,
*** there is a bit vector indicating which strides occur.  Easy enough, and
*** then Formula_For_Ak would check the bit vector.  I'm using this formula
*** because it works best in the common case of 3i, 3i+1, 3i+2 that we see
*** in register unrolling.)
***
*** [In any case, this is not fixed in the current version of the cache 
*** model. (RJC 30-JUN-1997).  I'll also add that that the formula isn't
*** right for the case where the spread is close to the cache line size.
*** Instead of: 
***
***   X = min(|k_j|, cls+spread)
***
*** I'll contend that at least in the case of two references, we should 
*** use: 
***
***   X = min(|k_j|, cls+S)  where S = min(M, cls-M) and M = mod(spread, cls) 
***
*** To see this, note that the case that the references are cls-1 elements
*** apart is equivalent to the case where they are 1 apart, since now the  
*** second fetch of the first reference is 1 apart from the first fetch of
*** the second reference. 
***  
*** But, for the most part it is rare that the absolute value of any 
*** coefficient is greater than 1, and since the cache line size is 
*** generally about 4 or 8 or so elements, it is normally the case that 
*** X = (min|k_j|), and this whole discussion rarely matters.  RJC]
***
*** (2.4) Modification for Large Cache Lines 
***
*** In Michael's original model, identified a single loop as the stride 
*** one loop.  We used one formula for the stride one loop, which included
*** the edge effects from the cache line, and another formula for the other
*** loops which did not include this term.  So, for example, if we are 
*** analyzing: 
***
***	do j = 1, n 
***	  do i = 1, n
***	    a(i,j) = 0 
***	  end do 
***	end do 
***
***  With a cache line size of 8 elements/cache line.  We would compute 
***  the footprint of a(i,j) in the two loops as (TC_i + 7)*TC_j, where 
***  TC_i is the trip count of the "i" loop and TC_j is the trip count 
***  of the "j" loop. Here the stride one loop "i" contributes (TC_i + 7) 
***  while the non-stride-one loop "j" contributes TC_j. 
***       
***  In the case of three loops: 
***   
***	do k = 1, n
***	  do j = 1, n 
***	    do i = 1, n 
***	      a(i,j,k) = 0 
***	    end do 
***	  end do 
***	end do 
***
***  The footprint is (TC_i + 7)*TC_j*TC_k.  Notice here that both "j" and
***  "k" contribute the same factor to the footprint when TC_j == TC_k == n.
***  If we based our decision solely on the footprint, then, we might con-
***  clude that having "k" as the second innermost loop is just as good as 
***  having "k" as the second innermost loop, which is generally not the 
***  case.  This is because while neither "j" nor "k" is stride one, "j" 
***  is closer to stride one than "k" is.  
***
***  Prior to the modification I'm about to describe, we still chose the 
***  (k,j,i) order as the best order because we marked both the "i" loop 
***  (which was stride one) and the "j" loop, which was the next most 
***  stride one loop, as being reuse loops, and so we included "i" and "j"
***  in the tile, while "k" was in the tile only when it was the required
***  inner loop (and so lost because it was not stride one).  However, 
***  the footprint for the choices (j,k,i) and (k,j,i) were the same.  
***  (We just never looked at (j,k,i) as a choice.) 
***
***  The above technique worked fine for the case of three loops, but 
***  fell apart for the case of four or more loops.  In this case, it 
***  was not possible to distinguish (l,k,j,i) from (k,l,j,i).  Also 
***  in the parallelism model, where we arbitrate cache costs solely based
***  on the footprint, we failed to see the desired differentiation. 
***
***  So, we've done the following.  For non-stride-one loops, we now 
***  compute an edge effect penalty which is equal to (cls-1)/PROD(TC_i)   
***  where TC_i are the trip counts of all of the loops with strides 
***  smaller than this loop. 
*** 
***  For example, conside the three nested loop above, and look at the 
***  (j,i,k) order.  Suppose the trip counts of these loops are (100,v0,v1).
***  For "j" our contribution was 100, but is now 100 + 7/v0.  Likewise 
***  for "k" our contribution was v1, but is now v1+7/(100*v0).   
***
***  This is by no means exact, but it does let us differentiate between
***  our various loop choices.  It is especially important when the cache
***  line is large (say for the L2 cache, which has a line of 32 single
***  precision or 16 double precision elements.                   
***
***  To implement this, we keep an array called Stride_Loops[] in each 
***  RG structure.  Stride_Loops[0] is the stride one loop,  Stride_Loops[1]
***  is the next largest stride loop, etc., through all of the loops. 
***  I use this array when I'm computing A_k for a non-stride one loop, 
***  it tells me which loops have lower stride than the "k" loop. 
***  
*** (2.5) When the Innermost Loop Doesn't Need to Be Blocked 
***
*** In one important case, adding cls to account for edge effects is too 
*** pessimistic.  What if we had a[i,j], and j isn't blocked so that it 
*** iterates over everything, and i is in the tile, too.  In this case (only),
*** It is enough to use (N_j-1)*X+cls/N_i rather than (N_j-1)*X+cls.  We
*** divide cls by N_i because there is only one edge effect to account for
*** over all of the iterations of i rather than N_i edge effects. 
***
***
*** For example, if i is in the block, a stride one loop, and dimension 
*** a[D_i, D_j], then for the reference a[i,j]:
***
***     A_j = (N_j-1)*X < D_j ? (N_j-1)*X + cls :
***                             (N_j-1)*X + cls/N_i
***
*** That was the simple case: no reuse from different references.  Now consider
*** reuse from distinct references.  First note that the number of occurences
*** within a loop of a[i] are irrelevant.  Either the leading one hits or not,
*** and then all other ones hit.  So loop body instance counts of a reference
*** don't matter, and neither do reads and writes.  Actually, writes cost more
*** but we ignore that.  Also, there is little reuse between x[i] and x[j],
*** to first order, or even between x[i] and x[i+m] (unless m is small, but
*** if it's a constant we have already discovered that by now, most likely).
*** There is only significant reuse amongst UNIFORMLY GENERATED REFERENCES,
*** those that are identical in form except possibly for a compile-time
*** integer constant.  These observations hold whether or not there is more
*** than one element in a uniformly generated reference.
***
***
*** (2.6)  Accounting for Multiple References  
***
*** The new concept for group reuse is reference groups.  We do not exactly
*** use the Carr, et. al. concept, since our approach is much more general
*** and suitable for blocking (and wrapping fish).  First of all, let's give
*** the punch line.  The formula changes only for A_k:
***
***	A_k = N_k + Max_k - Min_k,             for k, a non-"stride one loop"
***	      (N_j-1)*X + cls + Max_j - Min_j  for j, the "stride one loop"
***
*** (Actually, as before, if (N_j-1)*X+Max_j-Min_j >= D_j, then divide the 
*** cls in the stride one loop formula by N_i, where "i" is the next outer 
*** stride one loop.)
***
*** That formula is not for a reference, but for a reference group (for a 
*** single reference, Max-Min is always zero, giving the answer above when 
*** there is only one element in the group).  The footprint is simply the sum 
*** over reference groups.  So let's discuss how references are grouped into 
*** reference groups, and how Max and Min are chosen.  First, though, notice 
*** how the formula has changed.  Pairs of references touch identical elements 
*** except at the edges of the iteration space, in effect resulting in extra 
*** iterations, although in the stride one case, only extra elements of a 
*** cache line are used.
***
*** For example, consider a(i,j), a(i+1,j) and a(i,j+1).  It turns out that
*** these will all go in the same ref group for a doubly nested loop over i
*** and j.  If it were just a(i,j), it would use (N_i-1+cls)*Nj elements.
*** With the other two references, instead the answer is (N_i+1+cls)*(N_j+1) 
*** elements.  As you see, two more elements per iteration of the i loop 
*** (probably less than a cache line), but an extra row of data is needed.  
***
*** Note that this formula I've presented is cute and reasonably accurate, 
*** but is very much a heuristic.  For example, if to the above set of 
*** references we added a fourth, a(i+1,j+1), it turns out that the max and 
*** mins wouldn't change.  The actual answer would be that we use only one 
*** additional element, less than a cache line, in the doubly nested loop, 
*** so our error is tiny.  Still, its a heuristic.
***
*** Reference groups are found as follows.  If the references are not
*** uniformly generated, they go in different reference groups.  If they
*** are, but H_s*i = c2_s-c1_s has no solution, they also go in different
*** refgroups (cannot use the same cache line).  E.g. a(j,i,i+1) and
*** a(j,i+1,i+1) go in different refgroups, because there is no solution
*** to 
***
***    [1 0][i] = [1]
***    [1 0][j]   [0]
***
*** [Here are some details of the calculation: 
***
***       [0 1]                    [0]       [0]
***   H = [1 0]  H_s = [1 0]  c2 = [1]  c1 = [0]  c2_s = [1]  c1_s = [0]
***	  [1 0]        [1 0]       [1]       [1]         [1]         [1] 
***
*** In each case, we drop the first row out of H, c2, and c1 to form H_s,
*** c2_s, and c1_s, because the each row corresponds to a component in the
*** array reference, the first row corresponds to the first component and 
*** since the arrays are in column major order, this is the stride-one 
*** component. RJC]
*** 
*** 
*** (2.7) Implementation of Reference Groups in this Code
*** 
*** In the code below, an RG (short for "reference group", although that's
*** an oversimplification) holds a list of reference groups.  The reference
*** groups in the list would all fall into the same reference group, but
*** the constants in the index expressions vary too much.  For example,
*** a(i) and a(i+100) perhaps do not belong in the same reference group,
*** because in a typical blocked loop there will not be reuse.  Otherwise
*** they would go in the same reference group.  So an RG holds all references
*** that would go in the same reference group if the constants were small
*** enough.  Each RG_NODE is a member of the list.  These RG_NODEs do not
*** actually have lists of references.  Instead they have spreads in each
*** loop index dimension.  A reference falls within 20 iterations of another
*** reference in all dimensions, it is deemed to go in the same reference
*** RG_NODE (ie reuse is achieved); otherwise, it goes into a different RG_NODE
*** in the same RG.  This way, coalescing of RG_NODEs is easy: if we have
*** a(i) and a(i+25), which are in different groups, it's easy when we see
*** a(i+12) to combine all these into one RG_NODE.  It turns out that that's
*** the right thing to do when one examines the model.  
***
*** TODO OK: note that 20 is a heuristic.  Really, the right number is the 
*** blocksize, the number of iterations, possibly different for each dimen-
*** sion.  That number is often unknown.  20 is a good guess, and will lead 
*** to reasonable answers in most cases, especially since programmers do not 
*** often write a(i) and a(i+20) without intervening references, and when 
*** they do, it's often more like a(i) and a(i+1000) in which case we do the 
*** right thing anyway.  It's not a big issue. (Note: MAX |x_i-y_i| is the 
*** norm being used to get the distance.	       i 
***
*** Let's look more closely within an RG_NODE.  Each dimension has a max
*** and a min, the distance in the iteration space from the representative
*** of the RG.  (Each RG has one representative, so that a new reference
*** need only be compared to the RG, not to each element within it, to
*** understand where reuse occurs.)  Here's an example of the process:
***
***	a(j,i,i)	-> Max_i = 0, Min_i = 0, Max_j = 0, Min_j = 0
***
*** For the first element, the maxs and mins are all zero, since the distance
*** in the iteration space from a(j,i,i) to a(j,i,i), the representative for
*** this RG, is zero.  Additional elements will show how i and j differ from
*** this representative.  A tally of max and min in stride one constants
*** is kept.
***
*** TODO OK: This is only a simple heuristic, not completely accurate.  
*** We may not actually merge this with prefetching, since the assumptions 
*** may be different.  The footprint must be multiplied by (cls+max_s1const
*** -min_s1const).  Two nodes that differ by more than the cache line size 
*** go in different reference groups, but only if dim(Ker H) != dim(Ker H_s).
***
*** Anyway, now add in a(j,i,i+1).  We saw this above,  It goes in a different
*** RG entirely since it cannot touch the same cache line and the previous RG.
*** So let's just forget about it -- it's independent.  (We keep a stack of
*** RG for each array base, and compute the footprint for each array base
*** before going on to the next.)  
***
*** Suppose next we see a(j,i+1,i+1).  It goes in the same RG as the first. 
*** Solving H_s*[i,j] = [1,1], we get i = 1 as an answer. j could be anything,
*** and we use zero in that case.  So the first reference group now has
*** 
***	a(j,i,i)	-> Max_i = 1, Min_i = 0, Max_j = 0, Min_j = 0
***
*** To get the stride difference between a(j,i+1,i+1) and the representative
*** a(j,i,i) along the stride one dimension, let i_2 = [i,j] = [1,0] be our 
*** solution to the above equations.  The stride difference is given by 
*** computing the first component of H * i_2 + c_2 - c_1, where c_2 is the 
*** constant term for the new reference, and c_1 is the constant term for 
*** the representative reference.  In this case, we get [0 1]*[1 0]+0-0 = 0.
*** If we had been analyzing a(j+1,i+1,i+1) against a(j,i,i), we would have 
*** obtained [0 1]*[1 0]+1-0 = 1.  In any case, this formula for the stride:
***
***	stride = H * i_2 + c_2 - c_1  
***
*** works in general. 
***
*** We continue to add in references, consolidating whenever each of the mins 
*** for a particular reference come within 20 of all of the maxes.  For exam-
*** ple, if we see b(i,j) and b(i,j+30), these go in a SIMILAR_REFERENCE_GROUP,
*** but different REFERENCE_GROUPS.  If we now see b(i+2,j+15), we combine it
*** with the first, and the two resultant reference groups are now within
*** 20 in all dimensions, so all three form one reference group.
***
***
*** (2.8) Computing "d" in the Requirement Formula 
***
*** With this result, we can compute F1 and F2.  The required distance "d"
*** is precisely max(Max_i1 - Min_i1) over all the reference groups.  That's 
*** all the theory we need to compute misses per iteration for any blocksizes.
***
***
*** (3.0) THE BLOCKING MODEL
***
*** (3.1) The Square and Rectangular Blocking Models 
***
*** We started out with the idea of having two models, a fast one, which 
*** implemented "square blocking" and a slower, but more versatile one, 
*** called the "rectangular blocking" model.  It turned out that the cost
*** of executing the rectangular blocking model was always acceptable, so 
*** the code below only implements that model. 
***
*** Remember that for N deep loops, we transform to 2*N-1 loops, so there 
*** are N-1 blocking factors to choose.  For example, a three deep loop nest 
*** has two blocking factors.
***
*** In the square model we would choose the same blocking factor for both Bs.
*** Also, for speed reasons is did not distinguish between
***
***     do i by B
***        do j
***           do i = 1, B
***
*** and
***
***     do j by B
***        do i
***           do j = 1, B
***
*** That is, it didn't care which loop is innermost and does not choose
*** different B for the different cases.  Very fast, but not too precise.
*** Meanwhile, the rule for choosing B was that 15% (machine dependent)
*** of the cache would be used.  Period.
***
*** The rectangular model, which is currently used for all of our cache 
*** modeling, is slower than the square model but produces a better result.  
*** Here are the differences from the square model:
***
***   - May result in different B for >=3 deep loop nests.
***   - Loop overhead model included in search, so inner loop B may be 
***     larger and more than 15% of the cache may be used.  The model 
***     iterates to find a minimal cost solution in cycles/iter rather 
***	than aiming for 15% of the cache.
***   - Doesn't reuse previous answers for different inner loops.
***
***
*** (3.2) Computing the Loop Overhead 
***
*** In order to rectangular tile effectively, one has to understand loop
*** overhead.  How can we compute loop overhead?  We'll do it very simply,
*** and improve it later.  We identify three costs which together compose
*** the loop overhead: 
***
***   - Setting up the addresses:  For now, let's assume 3 cycles
***     per distinct reference passed into the cache model.
***   - Software pipelining startup: For now, let's assume 10 cycles.
***   - Register effects (unnecessary copy/restore at loop boundaries)
***     Assume 0 cycles.  Consider this a part of software pipelining 
***     startup.
***
*** [This is not actually the way that we model loop overhead today.  I'll
*** go through the code at some point and write a description. RJC] 
*** 
*** 
*** (3.3) Solving for Block Sizes by Recursive Descent 
*** 
*** So how do we solve for the block sizes?  Recall that the formula for
*** the footprints is a formula whose unknown values are the yet to be 
*** chosen blocksizes.  The formulas for the footprints are used in an
*** overall objective function which gives the cycles per iteration due 
*** to cache and loop overhead.  Our goal is to find the blocksizes which 
*** minimize this function. 
*** 
*** To do this, suppose we are solving for m blocksizes: B1, B2, ..., Bm.
*** We minimize this by recursive binary search.  We choose an arbitrary
*** value for B1, reducing the number of unknowns in our formula to m-1.
*** We then solve for the minimum of this reduced function.  We then at-
*** tempt a different value of B1, form a new formula of m-1 variables, 
*** and minimize this.  If this minumum is smaller than the previous, we
*** retain it, otherwise we retain the original.  We continue until we 
*** find a best value for B1.  The solution of the formula for m-1 vari-
*** ables will then yield the values of B2, B3, ..., Bm.  Note that by 
*** applying this technique recursively, we will eventually end up sol-
*** ving an equation in one unknown Bm, which can be done by simple binary
*** search.     
***
*** Recall that for an n-deep loop nest, there are n-1 variables, so for 
*** a three-deep nest, there are only two levels of binary search, which 
*** is cheap enough.  For deeper nests, we don't actually solve for unique
*** values of B1, B2, ..., Bm, but rather use the same value for B2 = B3 = 
*** ... = Bm. 
***
***
*** (3.4) Testing Different Loop Orders 
***
*** We run the cache model once for each possible inner loop.  Loops are 
*** included in the tile if only if they are judged to have reuse. If we  
*** are computing blocksizes for a two deep nest, we will then have at most 
*** four combinations to evaluate:  {1}, {0,1}, {0}, and {1,0}.  Here the 
*** loops are numbered 0 and 1, the order inside the curly braces is signi-
*** ficant, and the loops listed are those which participate in the tile.  
*** The first of the loops in each list inside the curly braces is the 
*** middle loop, the last is the innermost loop. 
***
*** For three loops, there are at most 2^2 * 3 = 12 combinations to evaluate; 
*** for four loops, this rises to 2^3 * 4 = 32.  At some point this becomes
*** prohibitively expensive.  So for loop nests which have five or more 
*** loops with reuse, we choose the best four loops (in some sense) and 
*** only evaluate these as being part of the tile.   
***
***
*** (3.5) When the Footprint Fits in the Cache
***
*** Important note: Our model is only accurate if data does not fit in the
*** cache.  If the SNL fits in the cache, then the cost is very low.  We
*** currently model it as zero, and thus all configurations of loops are
*** equally good.  So the first thing we do is see that the data used overflow
*** the cache.  If not, return 0.  If so, then we consider subnests to block.
*** Within a subnest, if it overflows the cache, then we can model it just
*** fine.  If not, then there's no point blocking those loops. But we still
*** need an estimate in cycles per iteration, so we can compare this subnest
*** with others, so that we can find the minimum cost subnest. 
***
*** If we don't overflow the cache, it may be possible that by adding 
*** additional outer loops we will reach a point where the loop nest over-
*** flows the cache, and we can come up with an appropriate cost in cycles
*** per iteration.  This is what we are doing in Compute_Actual_Miss_Bytes().
*** We first test to see if the subnest fits in the cache.  If it does, 
*** our model is accurate and we call Compute_Miss_Bytes() to get the 
*** appropriate formula for the miss bytes.  If it doesn't overflow the cache,  
*** it additional loops to until we reach a point where the nest doesn't fit
*** in the cache, and so have satisfied this criterion of our model.  
***
*** Note that if we must add an additional loop to get a subnest which  
*** doesn't fit in the cache, there is no point in blocking the loops 
*** further in, since they already fit in the cache (or the added loop, 
*** since it is the middle loop).  So, we aren't actually evaluating any 
*** blocking choices by doing this, just getting an estimate of the cycles
*** per iteration. 
***
*** In an earlier implementation of the cache model, a subnest which fit
*** in the cache simply returned "fits in cache", and Nest_Model() was 
*** assumed to explore the next configuration out.  But when blocking is 
*** restricted, or pragmas are used, or whatever, then this approach failed.  
***
*** The current approach has been implemented in a fairly cheap way.  
*** It is often the case that the loop that is added has no reuse, and 
*** Compute_Miss_Penalty() is extremely fast in that case.
***
***
*** (3.6) Blocking For Multiple Levels of Cache   
***
*** Yet another note.  The array permute_order[0..depth] holds the
*** permutation transformation we will need to apply to achieve the
*** desired transformation.  So permute_order[0] is loop that will be
*** outermost after permutation, but before tiling.  So, for example,
***
***                    do i'
***    do i      ->     do j
***     do j             do i
***
*** involves an interchange first, then a blocking.  We also have a data
*** structure to indicate the strips (the blocking).  But during computation,
*** we need more.  Essentially, we need to know which loops are available
*** for blocking, particularly when we block for the L2 cache after having 
*** blocked for the L1 cache. 
***
*** To see why, supopose we have blocked for the L1 cache as in the above
*** example.  When we go to block for the L2 cache, the loop order looks 
*** like {i',j}, because we can only block from the outermost loop down to 
*** and including the middle loop.  Note that if we had blocked the above
*** nest like: 
***                    do j'
***    do i      ->     do i
***     do j             do j
***
*** The available loops would be {j',i}, in that order.  The tile inside
*** important, but it's nothing that L2 can transform.  L2 basically thinks 
*** the loop nest is really two deep in this case, with j only available in 
*** chunks equal to the blocksize chosen for the L2 cache.  So we note that:
***
***                             do k'
***                              do l'
***    do i      ->               do i
***     do j                       do j
***      do k                       do k
***       do l                       do l
***        do m                       do m
***
*** the set of loops we can transform at the next level is {k',l',i}.
***
*** We use an array called available_order[] to store the list of the 
*** loops which are available for transformation at this level of the 
*** cache.  (More precisely, the depths of the available loops are what 
*** is stored in this array.)  There are "available_depth+1" elements in 
*** available_order[].  Upon entry to the cache model, available_depth
*** == depth and available_order[] == permute_order[], which is the iden-
*** tity permutation.  
***
*** In addition to available_order[], the multi-level blocking algorithm
*** uses a variable 'stripdepth' to keep track of which loops are part of 
*** an inner blocking nest and cannot be blocked further.  While blocking 
*** a nest of loops for the L1 cache, the value of stripdepth is always 
*** the depth of the innermost loop (in its original position) plus 1. 
*** So in the above example loop nest (i,j,k,l,m), stripdepth == 5 while
*** blocking for L1.  When blocking for the L2 cache, stripdepth may be
*** some value less than it was for L1.  In the above example, stripdepth
*** will be 1, because the (j,k,l,m) subnest cannot be block further.  
*** 
*** (3.7) Blocking for L1, then L2 
***
*** TODO: Note a very important result of our general approach.  We block
*** for L1, then block for L2 afterwards.  In other words, One_Cache_Model() 
*** is called for each cache level sequentially, rather than including all 
*** the caches in one model.  Consequently, we optimize for L1 first and 
*** leave L2 to clean up the mess.  
***
*** This means that we are essentially saying that modelling for L1 is more 
*** important than modelling for L2.  This is will be true, if L1 misses are 
*** usually just as important as L2 misses, and the data fits in L2 but  
*** not in in L1.  
***
*** Still, when things don't fit in L2, there can be modelling problems 
*** because a blocking that worked for L1 might be bad for L2.  This is a 
*** real problem and occasionally we block for L1, when it would have been 
*** better to not block, because the blocking makes things worse for L2.  
*** There's a dilemma here: if we model for both L1 and L2 simultaneously, 
*** then we are suboptimal when things DO fit in L2, which is quite common.
*** In the other hand, the cost of a cache miss for L2 is often many times
*** the cost of an L1 cache miss, so the effect of blocking for the L1
*** cache when the data doesn't fit in the cache will really be in the  
*** noise. 
***  
*** What should we do?  A run-time test?  Then we'd also have to write all 
*** the code to analyze L1 and L2 simultaneously, or somehow return lots of 
*** possibilities from the L1 modelling for the L2 to choose amongst.  So I 
*** think what we have now is good enough.  
***
*** [As a side note, applications people at SGI who hand block our codes 
*** generally block for the L2 cache and don't even bother with blocking 
*** for the L1 cache.  RJC] 
***
***
*** (4.0) ADDITIONAL ASPECTS OF THE MODEL 
***
*** (4.1) Overlapping Memory Misses with Machine Cycles 
***
*** Another note.  When the miss rate is deemed to be adequately low, then
*** blocking is not necessary because it can all be hidden anyway.  The
*** cache parameter Cur_Mhd->Load_Op_Overlap tells us what fraction of the
*** miss penalty can be overlapped with machine cycles (e.g. it's zero for a
*** non-blocking cache).  Meanwhile, Typical_Outstanding tells us how many
*** memory ops typically do execute simultaneously.
***
*** If almost all the memory misses can be overlapped with machine misses,
*** then of course the cache penalty is very low.  But, also, we count
*** the TLB penalty less in this case if we're not blocking.  The trouble
*** is that our TLB model is not very good, and especially not too good when
*** we are not blocking.  (Edge effects problems.)   So if we're not blocking
*** and the cache miss rate is low anyway, then we're mostly content to not
*** block, so we reduce the untrustworthy TLB.  Is this a reasonable way to
*** do things?  We also have the cache model parameter that indicates to what
*** extent we trust the TLB in the first place (0 is no trust, 1.0 is full
*** trust).
***
***
*** (4.2) Clean and Dirty Miss Penalties 
***   
*** Another note.  We have a clean miss penalty and a dirty miss penalty.
*** While it's hard to know which misses will be clean or dirty, we can
*** figure out the proportion fairly easily.  For example, suppose we have:
***
***        do i
***	     do j 
***            a(i,j) = a(i,j) + a(i+1,j)
***
*** Here, if the arrays are big, then all the misses will be dirty, because
*** the cache will be filled with lines of a, pretty much all of which will
*** have been written.  If there was also a b(i,j), then half the cache would
*** be a and half b, so roughly half the misses would be dirty.  
***
*** So, to figure out the proportion of clean misses to dirty misses, we 
*** compute the footprint for each uniformly generated set.  If there is 
*** any write in that set, the set goes into the "write footprint" COMPUTE_
*** FOOTPRINT_RVAL::WFormula) -- otherwise it goes into the "read footprint" 
*** (COMPUTE_FOOTPRINT_RVAL::RFormula).  This would be more precise if we 
*** only included in WFormula those references that were actually written, 
*** and put the remainder of the footprint for that uniformly generated set 
*** into the "read footprint", but it doesn't  much matter all that much.)
***
*** So the miss penalty used is an interpolation between the clean miss
*** penalty and the dirty miss penalty, based on what proportion of the
*** footprint is read but not written.  (TODO: This is okay, but not great.
*** For example if a data item is written over and over, it may be kicked
*** out of the cache repeatedly, so there may be several dirty writes.  The
*** footprint, then, does not tell the whole story.)
***
***
*** (4.3)  Hiding Cache Misses in Computation Time 
***
*** Another detail: cache misses can be hidden to some extent by computation
*** time.  How much is stated in cache parameters.  There are two numbers in
*** the cache parameters.  If they are both 0, then there is no overlap.
*** Both numbers are between 0 and 1.  1 means fully overlapped (to the extent
*** that there are computation cycles to hide the cache misses).  If the first
*** is 1 and the second is 0.5, that means the first cache miss cycle is
*** completely overlapped, and the cache miss cycle that overlapped the last
*** computation cycle is half hidden, and it's linear in between.  That's
*** the model.  
***
*** The result is that if you have more cache miss cycles than computation 
*** cycles, of course can only hide up to "computation cycles" and with the 
*** parameters 1 and 0.5, the actual number of cycles you do hide is only 3/4 
*** of that.  If you had a third as many cache miss cycles as computation 
*** cycles, then you'd hide all of the first cache miss cycle and 2/3*1 + 
*** 1/3*0.5 (linear interpolation) = 5/6 of the last cache miss cycle, or 
*** on average 11/12 of the cache miss cycles.  You're better at hiding if 
*** you have fewer cycles to hide.
***
***
*** (4.4) The Power of Two Hack Code:  Hitting the Same Cache Line 
***
*** And another: There is the "power of two hack" code.  If the leading
*** dimension is near a big enough power of two, then we assume that reuse
*** in the middle loop is highly restricted.  We are concerned that the 
*** successive references to array elements may hit the same cache line.  
***
*** In recent releases, especially in the interprocedural optimizer, we 
*** pad arrays which have leading dimensions that are close to a power of
*** two.  This means that the "power of two hack" is invoked a lot less 
*** often than it used to be.   
***
***
*** (4.5) How Non Constant Loops Effect the Footprint 
*** 
*** The cache model is based on computing footprints.  It computes the
*** number of bytes that would go in the cache for a certain set of loops
*** with certain iteration counts.  We have changed the footprint
*** computation code to not assume reuse for loops outside non_const_loops.  
***
*** For example, A[X<non_const_loops=1>, I, J] and A[X<non_const_loops=1>, 
*** I, J] looks like it has a dependence for I=0,J=0 but that's true only 
*** for loops at or inside depth=1.  If depth=0, then X might have changed 
*** and we don't know if there's reuse.
*** 
*** So in the footprint computation, each loop in the block that has a depth 
*** less than non_const_loops, we pretend that loop is not in the block, and 
*** just compute the footprint for the other loops.  Then we multiply the 
*** iteration counts for the bad loops to get the complete footprint.  
***
*** This computation is done for an entire uniformly generated set, which 
*** is okay; in practice, all these references will have the same value for 
*** non_const_loops, so it is reasonable to use the max(non_const_loops) of 
*** all references.  This will get us both spatial and temporal locality.)
***
***
*** (4.6) Modeling the TLB 
***
*** A TLB is typically fully associative, and if it's also FIFO, then exceed-
*** ing *the number of entries even by one can be catastrophic.  So we model
*** the TLB as follows.  If a cache has a TLB, then we compute the footprint
*** with a line size of the page size (modifying the cache model to understand
*** that with a(i,j) and a(i,j+1) may go in the same line if the entries of
*** the stride-one row is small).  The cost is zero when using less than the
*** TLB, because the TLB is fully associative and our model is conservative.
*** When we overflow the TLB, we charge a penalty that is somewhat less than
*** a full miss per each TLB element.  
***
*** The reason we don't charge a full miss when we overflow the TLB is 
*** that we don't believe the model enough to charge fully. The model is 
*** too approximate in ways that the regular cache model isn't.  We are 
*** assuming that the TLB looks like a normal cache with, say, 4096 byte
*** elements, and that is not quite accurate.  For example, if an array 
*** is of dimension (5,5,5,100), then a(0,0,0,0) and a(0,0,0,1) don't
*** end up on different lines.  The model misses this important case, and
*** so is not to be trusted.
***
*** More modelling detail.  The changes described above for long cache lines
*** are useful, and so we use them in that case, but it's still rare that an
*** array dimension will actually fit into a normal cache line.  For TLBs, 
*** where the line is huge, this change may kick in more often.  
*** 
*** This is even a problem in computing the footprint.  To see this, con-
*** sider a(i,j), a(i,j+1) and  suppose a(100,100).  Without understanding 
*** that N is small compared to the cache line size, we get the footprint
*** to be 8*N*(N+511), since there are N lines using N+511 elements, if a page
*** is 512 elements.  This is too pessimistic.  The actual footprint is
*** closer to 8*N*(N+1).  Can we get an estimate which is come closer to 
*** the correct value?  Yes, in certain cases.
***
*** What we do is this.  If N is known to be such that N*element_size <= 
*** page_size, that means that a(i,j) and a(i,j+1) may share the same cache 
*** line (i.e. tlb entry).  And in fact it barely matters how many
*** times i is executed.  So what we do is replace the element size by
*** N*element_size and ignore i totally, so in the above cases we'd model
*** it as a(j) and a(j+1).  TODO OK: This is not really adequate.
*** For example, if N is unknown, but we know we go from 1 to N-1 and
*** a(i,j) and a(i+1,j), then clearly we can pull this similar trick anyway.
***
*** To make it clear, the problem is this.  If we have a(i,j), then it's
*** modelled that each stride one line is (N+cache_line-1) elements,
*** which is an accurate estimate of the number of elements that will
*** be fetched given that a cache_line holds more than one element.
*** But if we also use r rows, then we get r*(N+cache_line-1).  This is
*** basically right, but more accurate is r*N+cache_line-1.  Also note
*** that our model is the better one when we are including blocking.  So
*** the trouble is that when N is small or cache_line is large, we have
*** big problems.  This fix treats the stride one row as a single element
*** when N<=cache_line, but doesn't fix anything otherwise.  It's helpful,
*** but not ideal. [I'm not clear on the meaning of this paragraph.  What 
*** is actually done in the code, and was is suggested as a future improve-
*** ment?  I'll add more comments once I've figured it out for myself. RJC].
***
***
*** (5.0) SOME SUGGESTED IMPROVEMENTS  
***
*** (5.1) Reusing the Footprints and Footprint Code 
***
*** Note that footprints are recomputed over and over, because there is an 
*** F1 and an F2.  So really best just to cache the result, and adjust the 
*** v1 and v2 variable names each time as appropriate. (One of those might 
*** have to change from vn to 100 when it becomes middle.)  This is espe-
*** cially important with 2-level caching.
***
*** [It's not clear to me that the compile-time issue here is very impor-
*** tant.  Compared to software pipelining, these algorithms are still 
*** cheap.  But I did manage to reuse the footprint code in the parallel
*** model, and it would be easier to work with if it had fewer parameters
*** and produced the value of the middle loop as a variable rather than 
*** the arbitrary constant value that is generally used now. RJC]
***
***
*** (5.2) Dealing with Unusual References 
***
*** (#1) References like A(B(i)) are bad, and are placed in arl->Num_Bad().
*** They are assumed hits, i.e. they are totally ignored.  That's okay.
*** But with U(i+j*(N+1)), there's actually a lot of information.  E.g.
*** i is stride 1, j isn't, and k doesn't appear at all.  That is, it's
*** close enough to U(i,j), that we can model it that way.  That would be
*** better than what we are doing now. 
***
*** (#2) The six references: s(j-1,k,l,1), ..., s(j-1,k,l,5) and s(j,k,l,m)
*** should not use six lines in the inner loop but rather 5 if that dimension
*** only has five, implying that m is between 1 and 5.
***
*** (#3) Given the two references a(m,1,j,l) and a(m,2,j,l), the model as-
*** sumes that each reference needs N_m+cls-1 elements in the stride one 
*** loop because of the long cache lines.  In this case, with adjacent 
*** constants in the next dimension, there shouldn't be a cls-1 term. (Such
*** "edge effects" are modelled properly in most cases.) 
***
*** (#4) In (#3) above, suppose m goes from 1 to 5, but is unrolled by 2.
*** So there are three iterations of the m loop.  The model uses 3 iterations,
*** and it sees 2m and 2m+1 after unrolling.  In other words, it models m going
*** 6 times, not 5!  This is fine for large loop bounds, but in this case,
*** it overestimates the required number of words.  With this, combined with 
*** the bad edge effects mentioned in (#3), if we assume a cls of 4, a line 
*** appears to use 9 words, not 5.
**/

// (6.0) READING THE CACHE DEBUGGING TRACE 
//
// (6.1) The STDOUT -Wb,-tt31:0x8000 Trace 
// 
// If you compile -Wb,-tt31:0x8000, you can get a trace of the cache model 
// execution.  This is printed to stdout.  More detailed information is 
// printed to TFile, and I've reorganized it to make it more readable, but 
// I don't describe that here. 
//
// For each choice of inner loop, we execute the Cache_Model() one time. 
// Each of these executions of Cache_Model() produces a trace similar to 
// the one below: 
//
//  Memory Level #0. Required inner #0.
//  *{0}:         Cache:    1.535 LpOver:  0.02993
//  *{3,0}:       Cache:   0.2533 LpOver:  0.04587 Blk: [0=64]
//   {2,0}:       Cache:    26.32 LpOver:    0.825 Blk: [0=4]
//   {2,3,0}:     Cache:   0.3998 LpOver:   0.3685 Blk: [3=660,0=8]
//   {1,0}:       Cache:   0.2685 LpOver:  0.04813 Blk: [0=64]
//  *{1,3,0}:     Cache:  0.07378 LpOver:  0.06381 Blk: [3=24,0=52]
//   {1,2,0}:     Cache:    25.99 LpOver:   0.8296 Blk: [0=4]
//   {1,2,3,0}:   Cache:   0.3521 LpOver:   0.4213 Blk: [3=630,0=7]
//    BEST:       Cache:  0.07378 DoOver:  0.03002
//  Memory Level #1. Required inner #1.
//  *{1}:         Cache:   0.7393 LpOver: 0.0003526
//  *{3,1}:       Cache:  0.02341 LpOver: 0.0003548
//   {2,1}:       Cache:   0.2119 LpOver: 0.0003966
//   {2,3,1}:     Cache:  0.02839 LpOver: 0.0004021 Blk: [3=192]
//   {0,1}:       Cache:   0.7393 LpOver: 0.0005288
//   {3,0,1}:     Cache:  0.02329 LpOver: 0.0007073
//   {2,0,1}:     Cache:   0.2123 LpOver: 0.0007492
//   {2,3,0,1}:   Cache:  0.02635 LpOver: 0.0007492 Blk: [3=192]
//  BEST:       Cache:  0.02341 DoOver:  0.03616
//  Adding bad reference bias:   0.5667
//  inner = 0 -> cycle est 1.73333 (before cache)  2.46337 (after cache)
//    0.663855 cache cycles  0.0661795 overhead cycles
//
// In this example, we have two caches L1 and L2, which are at Memory 
// Levels #0 and #1.  We execute One_Cache_Model() once for each trace 
// section starting with the words "Memory Level".  Hence, in this exam-
// ple, One_Cache_Model() is executed twice. 
//
// The required innermost loop is loop #0.  Note that at Memory Level #0, 
// all of the combinations in the {} on the left-hand side of the trace end 
// in for this reason.  
//
// Each line of the form:
//
//  *{1,3,0}:     Cache:  0.07378 LpOver:  0.06381 Blk: [3=24,0=52]
//
// represents a single execution of Nest_Model().  In the above example, 
// Nest_Model() is executed 8 times on the first call to One_Cache_Model()
// and 8 times on the second call to One_Cache_Model(). 
//
// In the Nest_Model trace line, the first item is the set of loops that 
// participate in the tile.  In the above example, {1,3,0} indicates that
// loops of depth 1, 3, and 0 participate in the tile.  This means that 
// the loops at depth 3 and 0 are blocked, and that the loop at depth 1 
// also carries reuse, but is not blocked. 
//
// Next we see "Cache:  0.07378".  This means that there are 0.07378  
// cycles due to cache misses per iteration of the innermost loop, given 
// this set of loops in the tile.  After that is "LpOver:  0.06381", 
// there are 0.06381 cycles due to loop overhead per itearion of the 
// innermost loop, given this set of loops in the tile. 
//
// Finally, on the line we see: "Blk: [3=24,0=52]".  This indicates that 
// the suggested blocking factors are 24 for loop 3 and 52 for loop 0. 
// Note that not all of the Nest_Model trace lines have a "Blk" component,
// those which don't are not blocked. 
// 
// The "*" before "{1,3,0}" indicates that this execution of One_Cache_Model()
// this is the best Nest_Model() choice that we have seen so far.  The cost 
// of the Nest_Model() choice can be determined by summing the "Machine" 
// "Cache" and "LpOver" numbers.  The choice with the minimum value is the 
// best nest model value.  In the above example, {1,3,0} is the best value
// for Memory Level #0. 
//
// Once we find the best value for a given memory level, we discard the 
// "LpOver" value and compute a new "DoOver" value, which we believe to 
// be more accurate.  This is done by calling Compute_Do_Ovehead().  In 
// the above trace we read: 
//
//   BEST:       Cache:  0.07378 DoOver:  0.03002
//  
// This summarizes the Cache and DoOver costs for the memory level #0. 
// 
// For this example, then, the loop nest is blocked for the L1 cache as: 
//
//   do loop-2 
//     do loop-3-tile 
//       do loop-0-tile 
//         do loop-1 
//           do loop-3 (block-size 24)
//             do loop-0 (block-size 52)
//
// We now consider blocking for the L2 cache (Memory Level #1).  Here the 
// required inner loop is #1, because the loops (2,3-tile,0-tile,1) are 
// the ones that we can no operate on, (3,0) have been placed innermost
// and will not be tiled again.  The trace shows that the best choice was:
//
//   *{3,1}:       Cache:  0.02341 LpOver: 0.0003548
//
// As before, the loop overhead was recomputed, giving us: 
//
//     BEST:       Cache:  0.02341 DoOver:  0.03616
// 
// The final loop nest will look like: 
//
//       do loop-2 
//         do loop-0-tile1 
//           do loop-3-tile1 
//             do loop-1 
//               do loop-3 (block-size 24)
//                 do loop-0 (block-size 52)
//
//  (If we choose the loop #0 to be the innermost loop.)
//
// When we have bad references in the loop, we add a factor to bias our 
// choice in favor of the required inner loop being the original inner 
// loop.  In our example, we do have bad references, and the required 
// inner loop was 3 != 0, the choice for this execution of Cache_Model().
// The bias factor is given by: 
//   Adding bad reference bias:   0.5667
// 
// The summary for this execution of Cache_Model() is given by: 
// inner = 0 -> cycle est 1.73333 (before cache)  2.46337 (after cache)
//   0.663855 cache cycles  0.0661795 overhead cycles
// This line also shows up in the normal -Wb,-tt31:0x4 trace. 
// It indicates that our best choice for a required inner loop of 0 has
// a per iteration cycle count of 1.73333 before cache and loop overhead 
// were analyzed by calling Cache_Model(), and a count of 2.46337 after 
// the cache and loop overhead figures are added in.  
//
// The part of this due to cache is 0.663855.  This comes from: 
//    0.07378  Memory Level #0 cache cost 
//    0.02341  Memory Level #1 cache cost 
//  + 0.5667   Bad reference bias 
//    ------   --------------------------
//    0.663855 Total cache cost 
//
// The part due to loop overhead is 0.0661795.  This comes from: 
//    0.03002    Memory Level #0 DoOver cost 
//    0.03616    Memory Level #1 DoOver cost  
//    -------    --------------------------- 
//    0.0661795  Total loop overhead cost 
// 
//  In this example, we have looked only at the trace for a single execution
//  of Cache_Model().  In reality, this is a 4-deep nest, and each of the 
//  four loops are legal innermost loops.  The summaries of the costs for 
//  each of these four innermost loops is: 
//
// inner = 3 -> cycle est 1.14286 (before cache)  1.14876 (after cache)
//    0.00494893 cache cycles  0.000953501 overhead cycles
// inner = 2 -> cycle est 1.1 (before cache)  2.07389 (after cache)
//    0.61118 cache cycles  0.362712 overhead cycles
// inner = 1 -> cycle est 1.1 (before cache)  1.73453 (after cache)
//    0.603523 cache cycles  0.031004 overhead cycles
// inner = 0 -> cycle est 1.73333 (before cache)  2.46337 (after cache)
//    0.663855 cache cycles  0.0661795 overhead cycles
// 
// The choice with "inner = 3" gives the minimum total cost of 1.14876,
// so this is the loop order that is finally chosen as best by the model.
// Its trace looked like: 
//
//  Memory Level #0. Required inner #3.
//  *{3}:         Cache:  0.04182 LpOver: 0.0006324
//   {2,3}:       Cache:  0.05215 LpOver: 0.001482 Blk: [3=2040]
//  *{1,3}:       Cache: 0.004968 LpOver: 0.003782 Blk: [3=680]
//   {1,2,3}:     Cache:  0.04617 LpOver: 0.001688 Blk: [3=1820]
//   {0,3}:       Cache:  0.04241 LpOver: 0.0006389
//   {0,2,3}:     Cache:  0.05213 LpOver: 0.001485 Blk: [3=2040]
//  *{0,1,3}:     Cache: 0.004949 LpOver: 0.003785 Blk: [3=680]
//   {0,1,2,3}:   Cache:  0.04618 LpOver: 0.001688 Blk: [3=1820]
//    BEST:       Cache: 0.004949 DoOver: 0.0009535
//  Memory Level #1. Required inner #0.
//    BEST:       Cache:        0 DoOver:        0
//  inner = 3 -> cycle est 1.14286 (before cache)  1.14876 (after cache)
//    0.00494893 cache cycles  0.000953501 overhead cycles
//
//  Which indicates that {0,1,3} was the best blocking for the L1 cache
//  and that no blocking was done for the L2 cache.  The final loop order
//  looks like: 
// 
//	do loop-2
//        do loop-3-tile 
//          do loop-0 
//            do loop-1 
//              do loop-3 (block-size 680)
//  
//  This is confirmed by looking at the -Wb,-tt31:0x4 trace which has: 
//   
//  Line 151: [Ivoie,Icapt,Icoef,Iecht --> Icoef(u=2),Ivoie,Icapt(u=7),Iecht 
//    block Iecht(680)[L1] outside Ivoie]
//
//  The original nest looked like: 
// 
//    do Ivoie = /* loop-0 */ 
//      do Icapt = /* loop-1 */ 
//        do Icoef = /* loop-2 */ 
//          do Iecht = /* loop-3 */  
//
//  and the transformed nest has: 
//
//    do Icoef (unrolled 2) = /* loop-2 */ 
//      do Iecht-tile = /* loop-3-tile */ 
//        do Ivoie = /* loop-0 */ 
//          do Icapt (unrolled 7) = /* loop-1 */ 
//            do Iecht (block-size 680) = /* loop-3 */  
//
//  (Comments and trace code by RJC) 
// 

/**
*** OTHER NOTES:
***
*** I use UINT64s to hold some depth-based bit fields.  This seems like a
*** reasonable bound -- the model just punts if the depth exceeds 62.
***
*** While the TT_LNO_CACHE_MODEL_DEBUG switch turns on debugging at run time,
*** you can control how much debugging it gives you with the file-macro
*** CM_DEBUGGING_LEVEL.  Set it to 0 to have TT_LNO_CACHE_MODEL_DEBUG have
*** no effect, to 1 to print out basic debugging info, to 2 for more etc.
***
*** est_iters (from Est_Num_Iterations)
***     used in computing do loop overhead (when we don't block)
***     used to guess number of iters in "middle loop" (not too important)
***
*** max_iters (from Est_Num_Iterations if not symbolic, and from Est_Max_Iters)
***     specifies the maximum allowed block size.
***     TODO OK: (not really okay, but not a big deal for now)
***        note, if est_iters=100, max_iters=500, then if we block
***        200 our model says we do 200 iterations.
***        But if we don't block, we model 100!
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define cache_model_CXX "cache_model.cxx"

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = cache_model_CXX "$Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <math.h>

#include "access_vector.h"
#include "model.h"
#include "cache_model.h"
#include "lnopt_main.h"
#include "alloca.h"
#include "lu_mat.h"
#include "vs.h"
#include "config_targ.h"
#include "snl.h"


// How much debugging info.  2 is a good place to start

#define CM_DEBUGGING_LEVEL              3

// 2 means BxBxBxBr.  1 means BxBx..xB which may not be square because
// these blocksizes are post-unrolled.  So in a 3 deep loop, if you unroll
// the middle by 5, then you will get a BxB block which is equivalent
// to a 5BxB block in terms of the number of iterations you do.  Set it
// to at least 2.  Setting it to 3 means that on four deep loops (the first
// point at which it makes a difference), you may spend a second or five
// searching for your answer (although if any of the loops have small loop
// bounds, you might be ok).  Test carefully before doing it.  You will never
// see more than a four deep loop, but if you do, setting this to 4 is asking
// for big trouble.  Don't do it.

#define MAX_DIFFERENT_BLOCKSIZES        2

// don't increase beyond 63 -- a UINT64 holds a bitvector for each loop
// in the do loop stack.

#define MAX_DEPTH                       62

// How scared we are of "middle" loop reuse.  Below the effective cache
// size, we are not very scared.  But setting this to zero causes all sorts
// of wierd effects, and having a fixed fear factor does very strange things
// for non-inner reuse.  As an example, suppose we have good stride one
// innermost use inside a loop, but we want to interchange to get good model
// performance.  If we have a fixed fear of outer loop reuse, we get one
// or the other, depending on the precise parameters.  But if instead we
// have fear based upon how much of the cache we use, we will probably have
// the best inner loop and some appropriate large but not too large tile
// size (exactly what we want).
// 
// Here's how the parameters below work.  Suppose we can use 10% of the cache.
// Then we lose 0% of the reuse in the middle loop if we use 0% of the cache,
// and if we use 10%, we lose 10%*0.20 = 2.0% of the reuse (assuming
// the BASIC_MIDDLE_PENALTY is 0.2).  After that,
// the additional penalty kicks in, so that at 15% cache utilization we
// lose 15%*0.20 + 5%*1.0 = 8% of the reuse (assuming the
// ADDITIONAL_MIDDLE_PENALTY is 1.0).  This is what the effective
// cache size of the cache is used for: determining when to kick in the
// big penalty.  (Note that the function is continuous -- the penalties grow
// after after the effective cache size, but there is no discontinuity.
//
// I'd be reluctant to lower this
// below 0.2.  0.25 might be better.  Also, note that this is for single
// processors.  For multi-processor execution, where each middle loop gets
// a processor, things look very different.  Whereas here we penalize middle
// loop stride one stuff, in the multi-processor case, a middle loop that's
// stride one is a very good thing, reducing false sharing.
//

#define BASIC_MIDDLE_PENALTY            0.20
#define ADDITIONAL_MIDDLE_PENALTY       1.00

// in a memory (not a cache), there's no concept of interference, or risk of
// interference, so no middle penalty.  The additional middle penalty should
// be very large.

#define BASIC_MIDDLE_PENALTY_MEM        0.00
#define ADDITIONAL_MIDDLE_PENALTY_MEM   2.00

// in a tlb (not a cache), fully associative, so no middle penalty
// until overflow the cache, and then large additional penalty.
// But we keep the numbers low anyway
// because we don't trust the tlb model that much -- it models things
// as a tiny cache with a huge cache line, and that magnifies errors in
// the model.

#define BASIC_MIDDLE_PENALTY_TLB        0.00
#define ADDITIONAL_MIDDLE_PENALTY_TLB   0.20

// We could iterate down to the very best blocksize, but who cares if it's
// 275 or 274?  This macro tells how close we wish to be.  A value of 32
// means that for blocksizes above 32 (in the case of rectangular, remember
// that we divided by the unrolling factor) we are willing to be off by 1,
// 64 off by 2, etc.  Within roughly 3%.  It's more efficient if these
// numbers are powers of two.  Given the uncertainties in the cache model,
// I would guess 32 good, and 16 probably too small.

#define RECT_BLOCKSIZE_UNCERTAINTY      32

// the maximum number of iterations

#define UNBOUNDED_ITERS                 12345678

// The maximum cache block size.  If cache sizes get to big we have
// LCM problems.   Also, we require all our blocksizes to divide the
// number MAX_LCM.  That way, we can guarantee we don't have any LCM
// problems.

#define MAX_BLOCKSIZE                  100000
#define MAX_LCM        (64*3*3*5*5*5*7*11*13*17)   /* 1.2 b: < (1<<31) */

// if there are any unanalyzable references, then we want to bias things
// toward the way the programmer wrote the code, because maybe the programmer
// knows something we don't.  So for each unanalyzable reference, how many
// cycles worse is it to permute than not?  The FIRST_BIAS is how much we
// penalize for the first bad ref, and the INCREMENTAL_BIAS is how much for
// each additional ref.

#define BAD_REF_CYCLE_FIRST_BIAS        1.5
#define BAD_REF_CYCLE_INCREMENTAL_BIAS  0.5

// If we have a[50*i+j], we want to treat it as a[i,j].

#define MAX_COEFF                       20

//----------------------------------------------------------------------------
// Static local variables that someday could be controlled with flags.
//----------------------------------------------------------------------------

// "Long_Line"s.  for example, if a[100][3], then a[i][j] and a[i+1][j] might
// share the same cache line.  (There is discussion of this in TLB comments
// above).  While this is slightly interesting for caches, it's very intresting
// for TLBs.  These variables control the extent to which we've enabled
// this feature.  Currently, it's enabled for TLB and cache both.  Why not?
// But for debugging, it can be convenient to turn it off.

enum LONG_LINE_POLICY   {LONG_LINE_OFF, LONG_LINE_ON,
                         LONG_LINE_TLB_ONLY, LONG_LINE_CACHE_ONLY};
static LONG_LINE_POLICY Long_Line_Policy = LONG_LINE_ON;
static INT32            Loop_Overhead = -1;

//----------------------------------------------------------------------------
// Static local variables (and one global variable) 
//----------------------------------------------------------------------------

       INT        Debug_Cache_Model = 0;
static INT64      Nominal_Blocksize[SNL_MAX_LOOPS+1];
static MHD_LEVEL* Cur_Mhd = NULL;
static INT        Rtry_Count;
static INT        Happy_Coefficient;        // if i+20*j where Happy<20, then punt
static INT        Max_Different_Blocksizes = MAX_DIFFERENT_BLOCKSIZES;

//----------------------------------------------------------------------------
// Utility functions
//----------------------------------------------------------------------------

#ifndef ABS
#define ABS(a) ((a<0)?-(a):(a))
#endif

static INT Divceil(INT a, INT b)
{
  return (a + b - 1)/b;
}


static BOOL Is_In_Array(INT val, const INT* array, INT array_sz)
{
  INT i;
  for (i = 0; i < array_sz; i++)
    if (array[i] == val)
      return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Set_Cache_Model_Statics
// FUNCTION: Set the values of Cur_Mhd, Rtry_Count, Happy_Coefficient,
//   and Fpool, since they are needed when calling Compute_Footprint() 
//   externally. 
//-----------------------------------------------------------------------

extern void Set_Cache_Model_Statics(INT mhd_level)
{
  Cur_Mhd = &Mhd.L[mhd_level]; 
  FmtAssert(Cur_Mhd->Valid(), ("Not a valid MHD level")); 
  Rtry_Count = 0; 
  Happy_Coefficient = MAX(10, 3*LNO_Outer_Unroll);
  Happy_Coefficient = MAX(Happy_Coefficient, 3*LNO_Outer_Unroll_Max);
  Happy_Coefficient = MAX(Happy_Coefficient, 3*LNO_Outer_Unroll_Prod_Max);
  Happy_Coefficient = MIN(Happy_Coefficient, 30);
  MAT<FRAC>::Set_Default_Pool(&LNO_local_pool);
  FORMULA::Fpool = &LNO_local_pool;
}

static void Print(FILE*      f,
                  char*      cs,
                  INT        depth,
                  INT        nstrips,
                  INT        stripdepth,
                  const INT* new_order,
                  const INT* available_order,
                  INT        available_depth,
                  const INT* iloop,
                  const INT* stripsz,
                  const INT* striplevel)
{
  INT i;

  fprintf(f, "*** cache model transformation information <%s, depth=%d>\n",
          cs ? cs : "", depth);
  fprintf(f, "*** new_order:");
  for (i = 0; i <= depth; i++)
    fprintf(f, " %d", new_order[i]);
  fprintf(f, "   *** available_order:");
  for (i = 0; i <= available_depth; i++)
    fprintf(f, " %d", available_order[i]);
  if (nstrips > 0) {
    fprintf(f, "\n*** strips <outerdepth=%d>:", stripdepth);
    if (striplevel) {
      INT s;
      for (s = 0; s < nstrips; s++)
        fprintf(f, " %d[sz=%d,lv=%d]", iloop[s], stripsz[s], striplevel[s]);
    }
    else {
      for (INT s = 0; s < nstrips; s++)
        fprintf(f, " %d[sz=%d]", iloop[s], stripsz[s]);
    }
  }
  fprintf(f, "\n");
}

//---------------------------------------------------------------------------
// FORMULA member functions
//---------------------------------------------------------------------------

MEM_POOL* FORMULA::Fpool = NULL;
double    FORMULA::_scratch[FORMULA::SCRATCH_REGISTERS];

static INT rprint_cnt;

double FORMULA::Eval(INT cnt, const mINT64* vars) const
{
  double* dvars = cnt ? (double*) alloca(cnt * sizeof(double)) : NULL;
  INT i;
  for (i = 0; i < cnt; i++)
    dvars[i] = (double) vars[i];

  if (Debug_Cache_Model >= 3)
    rprint_cnt = 0;

  double e = Eval(dvars);

  if (Debug_Cache_Model >= 3 && rprint_cnt)
    fprintf(TFile, "\n");

  return e;
}

double FORMULA::Eval(INT cnt, const mINT32* vars) const
{
  double* dvars = cnt ? (double*) alloca(cnt * sizeof(double)) : NULL;
  INT i;
  for (i = 0; i < cnt; i++)
    dvars[i] = (double) vars[i];

  if (Debug_Cache_Model >= 3)
    rprint_cnt = 0;

  double e = Eval(dvars);

  if (Debug_Cache_Model >= 3 && rprint_cnt)
    fprintf(TFile, "\n");

  return e;
}

double FORMULA::Eval(INT cnt, const double* vars) const
{
  if (Debug_Cache_Model >= 3)
    rprint_cnt = 0;

  double e = Eval(cnt ? vars : NULL);

  if (Debug_Cache_Model >= 3 && rprint_cnt)
    fprintf(TFile, "\n");

  return e;
}

double FORMULA::Eval(const double* vars) const
{
  Is_True(this, ("FORMULA::Eval() called with this == NULL"));

  double ans;

  switch (_fop) {
   case FORMULA_ADD:
   case FORMULA_SUB:
   case FORMULA_MUL:
   case FORMULA_DIV:
   case FORMULA_MAX:
   case FORMULA_MIN:
   case FORMULA_GE:
   case FORMULA_GT:
   case FORMULA_LE:
   case FORMULA_LT:
   case FORMULA_COMMA:
    {
      double l = _kids.Left->Eval_Inlined(vars);
      double r = _kids.Right->Eval_Inlined(vars);
      switch (_fop) {
       case FORMULA_ADD: ans = l + r; break;
       case FORMULA_SUB: ans = l - r; break;
       case FORMULA_MUL: ans = l * r; break;
       case FORMULA_DIV: if (r == 0.0 && Debug_Cache_Model) {
                           fprintf(TFile, "zero divide in formula: ");
                           Print(TFile);
                           fprintf(TFile, "\n");
                         }
                         FmtAssert(r, ("zero divide"));
                         ans = l / r; break;
       case FORMULA_MAX:  ans = MAX(l,r); break;
       case FORMULA_MIN:  ans = MIN(l,r); break;
       case FORMULA_GE:  ans = l >= r; break;
       case FORMULA_GT:  ans = l > r; break;
       case FORMULA_LE:  ans = l <= r; break;
       case FORMULA_LT:  ans = l < r; break;
       case FORMULA_COMMA: ans = r; break;
     }
    }
    break;
   case FORMULA_AND:
    if (_kids.Left->Eval_Inlined(vars) == 0.0)
      ans = 0.0;
    else
      ans = _kids.Right->Eval_Inlined(vars) != 0.0;
    break;
   case FORMULA_OR:
    if (_kids.Left->Eval_Inlined(vars) == 1.0)
      ans = 1.0;
    else
      ans = _kids.Right->Eval_Inlined(vars) != 0.0;
    break;
   case FORMULA_COND:
    if (_kids.Cond->Eval(vars))
      ans = _kids.Left->Eval(vars);
    else
      ans = _kids.Right->Eval(vars);
    break;
   case FORMULA_FCONST:
    ans = _fconst;
    break;
   case FORMULA_VAR:
    Is_True(vars, ("vars is NULL"));
    ans = vars[_var];
    break;
   case FORMULA_USE:
    Is_True(_var >= 0 && _var < SCRATCH_REGISTERS,
            ("Bad scratch register use %d", _var));
    ans = _scratch[_var];
    break;
   case FORMULA_SET:
    Is_True(_var >= 0 && _var < SCRATCH_REGISTERS,
            ("Bad scratch register set %d", _var));
    ans = _scratch[_var] = _kids.Left->Eval(vars);
    if (Debug_Cache_Model >= 4) {
      rprint_cnt++;
      fprintf(TFile, "[r%d=%.4g]", _var, ans);
    }
    break;
   default:
    FmtAssert(0, ("bad formula"));
    ans = 0.0;
    break;
  }

  return ans;
}

FORMULA* FORMULA::Add_To_Variable(INT var, INT val)
{
  Is_True(this, ("FORMULA::Duplicate() called with this == NULL"));

  switch (_fop) {
   case FORMULA_ADD:
   case FORMULA_SUB:
   case FORMULA_MUL:
   case FORMULA_DIV:
   case FORMULA_MAX:
   case FORMULA_MIN:
   case FORMULA_GE:
   case FORMULA_GT:
   case FORMULA_LE:
   case FORMULA_LT:
   case FORMULA_AND:
   case FORMULA_OR:
   case FORMULA_COMMA:
    _kids.Left = _kids.Left->Add_To_Variable(var,val);
    _kids.Right = _kids.Right->Add_To_Variable(var,val);
    return this;
   case FORMULA_COND:
    _kids.Cond = _kids.Cond->Add_To_Variable(var,val);
    _kids.Left = _kids.Left->Add_To_Variable(var,val);
    _kids.Right = _kids.Right->Add_To_Variable(var,val);
    return this;
   case FORMULA_FCONST:
    return this;
   case FORMULA_VAR:
    return FORMULA::Add(this, FORMULA::Const(val));
   case FORMULA_USE:
    return this;
   case FORMULA_SET:
    _kids.Left = _kids.Left->Add_To_Variable(var,val);
    return this;
   default:
    FmtAssert(0, ("Bad formula for Duplicate"));
    return NULL;
  }
}

FORMULA* FORMULA::Duplicate() const
{
  Is_True(this, ("FORMULA::Duplicate() called with this == NULL"));

  switch (_fop) {
   case FORMULA_ADD:
    return FORMULA::Add(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_SUB:
    return FORMULA::Sub(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_MUL:
    return FORMULA::Mul(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_DIV:
    return FORMULA::Div(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_MAX:
    return FORMULA::Max(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_MIN:
    return FORMULA::Min(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_GE:
    return FORMULA::Ge(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_GT:
    return FORMULA::Gt(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_LE:
    return FORMULA::Le(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_LT:
    return FORMULA::Lt(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_AND:
    return FORMULA::And(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_OR:
    return FORMULA::Or(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_COMMA:
    return FORMULA::Comma(_kids.Left->Duplicate(), _kids.Right->Duplicate());
   case FORMULA_COND:
    return FORMULA::Cond(_kids.Cond->Duplicate(),
                         _kids.Left->Duplicate(),
                         _kids.Right->Duplicate());
   case FORMULA_FCONST:
    return FORMULA::Const(_fconst);
   case FORMULA_VAR:
    return FORMULA::Var(_var);
   case FORMULA_USE:
    return FORMULA::Use(_var);
   case FORMULA_SET:
    return FORMULA::Set(_var, _kids.Left->Duplicate());
   default:
    FmtAssert(0, ("Bad formula for Duplicate"));
    return NULL;
  }
}

static INT precidence(FORMULA_OP fop)
{
  switch (fop) {
   case FORMULA_BAD:
    return -1;
   case FORMULA_COMMA:
    return 0;
   case FORMULA_SET:
    return 1;
   case FORMULA_COND:
    return 2;
   case FORMULA_OR:
    return 3;
   case FORMULA_AND:
    return 4;
   case FORMULA_GE:
   case FORMULA_GT:
   case FORMULA_LE:
   case FORMULA_LT:
    return 5;
   case FORMULA_ADD:
   case FORMULA_SUB:
    return 6;
   case FORMULA_MUL:
   case FORMULA_DIV:
    return 7;
   case FORMULA_MAX:
   case FORMULA_MIN:
   case FORMULA_FCONST:
   case FORMULA_VAR:
   case FORMULA_USE:
    return 8;
  }
  FmtAssert(0, ("Bad fop = %d", fop));
  return 0;     // shut up the compiler
}

inline BOOL assoc(FORMULA_OP fop)
{
  return fop == FORMULA_ADD || fop == FORMULA_MUL || fop == FORMULA_COMMA;
}

void FORMULA::Print(FILE* f, FORMULA_OP parent) const
{
  FmtAssert(this, ("FORMULA::Print() called with this == NULL"));

  BOOL  dont_parenthesize = ((_fop == parent && assoc(_fop)) ||
                             precidence(_fop) > precidence(parent));

  if (!dont_parenthesize)
    fprintf(f, "(");

  switch (_fop) {

   case FORMULA_FCONST:
    fprintf(f, "%.4g", _fconst);
    break;

   case FORMULA_VAR:
    fprintf(f, "v%d", _var);
    break;

   case FORMULA_USE:
    fprintf(f, "r%d", _var);
    break;

   case FORMULA_SET:
    fprintf(f, "r%d=", _var);
    _kids.Left->Print(f, _fop);
    fprintf(f, "\n"); 
    break;

   case FORMULA_ADD:
   case FORMULA_SUB:
   case FORMULA_MUL:
   case FORMULA_DIV:
   case FORMULA_GE:
   case FORMULA_GT:
   case FORMULA_LE:
   case FORMULA_LT:
   case FORMULA_AND:
   case FORMULA_OR:
   case FORMULA_COMMA:
    _kids.Left->Print(f, _fop);
    fprintf(f, "%s",
	       _fop == FORMULA_ADD ? "+" :
	       _fop == FORMULA_SUB ? "-" :
	       _fop == FORMULA_MUL ? "*" :
	       _fop == FORMULA_DIV ? "/" :
	       _fop == FORMULA_GE ? ">=" :
	       _fop == FORMULA_GT ? ">" :
	       _fop == FORMULA_LE ? "<=" :
	       _fop == FORMULA_LT ? "<" :
	       _fop == FORMULA_AND ? "&&" :
	       _fop == FORMULA_OR ? "||" :
	       _fop == FORMULA_COMMA ? "," :
	    (FmtAssert(0, ("Bad fop = %d to FORMULA;:Print()", _fop)), ""));
    _kids.Right->Print(f, _fop);
    break;

   case FORMULA_MAX:
   case FORMULA_MIN:
    // Print(f) prints without parens
    fprintf(f, "%s", _fop == FORMULA_MAX ? "Max(" : "Min(");
    _kids.Left->Print(f);
    fprintf(f, ",");
    _kids.Right->Print(f);
    fprintf(f, ")");
    break;

   case FORMULA_COND:
    _kids.Cond->Print(f, _fop);
    fprintf(f, "?");
    _kids.Left->Print(f, _fop);
    fprintf(f, ":");
    _kids.Right->Print(f, _fop);
    break;

   default:
    FmtAssert(0, ("Bad FORMULA::_fop %d", _fop));
  }

  if (!dont_parenthesize)
    fprintf(f, ")");
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Utility routines: Same_Ug() Same_Consts()
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

static BOOL Same_Ug(const ACCESS_VECTOR* av1, const ACCESS_VECTOR* av2)
{
  FmtAssert(!av1->Too_Messy && !av2->Too_Messy,
            ("Same_Ug(): a too messy access vector"));
  FmtAssert(av1->Nest_Depth() == av2->Nest_Depth(),
            ("Same_Ug(): bad depths %d and %d",
             av1->Nest_Depth(), av2->Nest_Depth()));
  INT i;
  for (i = 0; i < av1->Nest_Depth(); i++)
    if (av1->Loop_Coeff(i) != av2->Loop_Coeff(i))
      return FALSE;

  if (av1->Contains_Lin_Symb()) {
    if (av2->Contains_Lin_Symb() == FALSE ||
	!(*av1->Lin_Symb == *av2->Lin_Symb))
      return FALSE;
  }
  else if (av2->Contains_Lin_Symb()) {
    return FALSE;
  }

  if (av1->Contains_Non_Lin_Symb()) {
    if (av2->Contains_Non_Lin_Symb() == FALSE ||
	!(*av1->Non_Lin_Symb == *av2->Non_Lin_Symb))
      return FALSE;
  }
  else if (av2->Contains_Non_Lin_Symb()) {
    return FALSE;
  }

  return TRUE;
}

static BOOL Same_Ug(const ACCESS_ARRAY* aa1, const ACCESS_ARRAY* aa2)
{
  if (aa1->Num_Vec() != aa2->Num_Vec())
    return FALSE;
  INT i;
  for (i = aa1->Num_Vec() - 1; i >= 0; i--)
    if (!Same_Ug(aa1->Dim(i), aa2->Dim(i)))
      return FALSE;

  return TRUE;
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// RG_NODE: a reference group (summarized by max/mins)
// RG_LIST: a list of RG_NODE with the "representative" access vector.
//          E.g. a(i,j) and a(i+1000,j) go in different reference groups
//          because of the lack of locality, but they have the same
//          access_vector.
// RG_ITER: iterating through an RG_LIST.
// RG     : contains the RG_LIST and summary information
//          Thus a(i,j) and a(i+100,j) are in the sam RG.  To represent
//          an entire arl requires a stack of RG.
//
// RG_NODE:
//
// To be in the same reference group, two references must be of the form
//    ->  ->         ->  ->
// a(Hi + C1) and a(Hi + C2).  That is, they may only differ in the
// (compile time) constant terms.  We put them in different RG_NODES, but
// the same RG_LIST, if the constants differ by so much that reuse is
// limited.  If we discover there is some reuse (e.g. a(i) and a(i+100),
// but later see a(i+1)...a(i+99), then we merge them back into the same
// list.  (The reuse analysis is better if all nearby data are in the same
// group.  The right thing is done with a(i)...a(i+100), wereas if
// there were separate groups for a(i)...a(i+49) and a(i+50) and
// a(i+51)...a(i+100), they would be treated independently.)
//
// Given a(i,j) and a(i,j+1), it's easy to see that they belong in the same
// RG_NODE (there's reuse) *if j is a loop to be tiled*.  We
// can even see that Bi i's and Bj j's require Bi x Bj+1 space.  We'd
// represent this situation as Mn(i) = 0, Mx(i) = 0, Mn(j) = 0, Mx(j) = 1.
// Each loop in the tile gets a Mn and Mx.  Actually, the #loops and
// #dimensions don't always match.  So that's handled by solving
// Hx = (c2 - c1) where c1 is the "representative".  the answer x gives
// a value for each dimension of interest.
//
// A nuance.  One of the loops is the stride one loop.  Maybe.  There may
// be no stride one loop, but still a stride-one value (e.g. a(1,i)).
// Thus we're forced to keep a separate stride as well.  So what do we do with
// a(i,j) and a(i+1,j)?  The answer is that Mn(i)=0 and Mx(i)=0 when
// i is the stride one loop and just use Mx_Stride and Mn_Stride.  If there
// is no stride one loop then Mn_Stride and Mn_Stride are still significant
// to catch the a(1,i) case.
//
// Reference groups don't care about multi-level blocking, except that
// a(i,j+10) and a(i,j) go in the same group even if j is not a loop to
// be tiled, but was stripped down below, or even not stripped at all,
// but is included in the inner loop.  Thus RG must be passed all the
// inner loops (and optionally their expected number of iterations)
// to form the reference groups accurately.  That in addition to all the
// loops we plan to tile here.
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

class RG_NODE : public CHAIN_NODE {
  DECLARE_CHAIN_NODE_CLASS(RG_NODE);
 public:
  INT	Mn[SNL_MAX_LOOPS];	// TODO OK: a lot of wasted space.  Matter?
  INT	Mx[SNL_MAX_LOOPS];
  INT	Mn_Stride;
  INT	Mx_Stride;
  INT	Entries;

  RG_NODE(INT elements, const INT* mn, const INT* mx,
	  INT mn_stride, INT mx_stride);
  void Print(FILE* f) const;
  ~RG_NODE() {}
 };

class RG_LIST : public CHAIN {
  DECLARE_CHAIN_CLASS(RG_LIST, RG_NODE);
  friend class RG;
 public:
  RG_LIST(MEM_POOL* p, INT nloops, INT element_size, const INT*, const INT*,
          BOOL);
  ~RG_LIST();

  void		Insert(const INT* c, INT stride, BOOL has_store);
  void		Print(FILE*) const;

  MEM_POOL*	Pool;
  INT		Elements;
  INT		Element_Size;	// in bytes.
  INT		Count;		// distinct references
  INT		Store_Count;	// distinct write references
  INT		Stride_Loop;	// -1 if no stride one for single elt.
                                // Else index (in H) of stride one loop, so
                                // that loops[Stride_Loop] is depth of stride
                                // one loop.
  INT		Actual_Stride;	// only if Stride1_Loop is set.
                                // For example, in a[2i], this value is 2.
                                // In a[3i,0] and a[3i,1], where a is declared
                                // a[*,2], we have Stride_Loop=i, Stride_Row=0
                                // (not 1), and Actual_Stride=3.
                                // NOTE: Actual_Stride may be longer than
                                // the cache line size.  We still call that
                                // stride reuse, so need to see if
                                // Actual_Stride*Element_Size >= Line_Size
                                // before drawing any conclusions!
                                // Actual_Stride is basically
                                // (*H)(H->Cols()-1,Stride_Loop)
  INT           Effective_Element_Size; // When the Stride_Row isn't the last
                                // row, then the elements are effectively
                                // longer.
  INT           Next_Outer_Stride_Loop;   // e.g. a[i,j]: if j is stride one,
                                // and if i is included in the block, the
                                // edges of j are not wasted, but only if
                                // j is unblocked.
  mBOOL         Using_TLB;      // tells what the page size
  INT 		Stride_Loops[SNL_MAX_LOOPS]; // Stride_Loops[0] == Stride_Loop
				// Stride_Loops[1] == Next_Outer_Stride_Loop
				// Stride_Loops[i] == i-th next stride loop

 private:

  mINT32        Max_Diff[SNL_MAX_LOOPS];
  void          Simplify(BOOL first_only);
};

RG_LIST::RG_LIST(MEM_POOL* p, INT nloops, INT element_size,
                 const INT* loops, const INT* approx_iters, BOOL using_tlb)
    : Pool(p), Elements(nloops), Element_Size(element_size),
      Count(0), Store_Count(0),
      Using_TLB(using_tlb),
      Effective_Element_Size(element_size),
      Next_Outer_Stride_Loop(-1),
      Actual_Stride(-1), Stride_Loop(-1)
{
  INT i;
  for (i = 0; i < nloops; i++)
    Max_Diff[i] = approx_iters[loops[i]];
}

class RG {
 public:
  RG_LIST	Rglist;
  RG(MEM_POOL* p, ACCESS_ARRAY* aa, INT elements,
     const INT* loop, INT element_size, SYMBOL sym, const INT*, BOOL,
     WN*, INT, INT*);
  ~RG();

  MEM_POOL*	Pool;
  INT		Elements;               // number of loops we're examining

  // FALSE if no soln Hx=(c-C)
  BOOL		Insert(const ACCESS_ARRAY*, BOOL has_store, INT, const INT*);
  void		Print(FILE*) const;

  ACCESS_ARRAY*	Aa;			// representative's access array
  SYMBOL	Sym;			// for printing only
  BOOL          Is_Local_Array;         // whether this is a local array


  // this applies only to the representative
  INT*			C;		// constants, all integral

  // These apply to all members of the list.
  // If H has large coeffs, it's typically from linearization, in which case
  // analysis is trickly.  Worse, the fractions in the kernel matices can
  // overflow.  So we have to play a trick.  If we have a[100*i+j], we treat
  // it loke a[i,j].  See MAX_COEFF.  So this can add extra rows to H.
  // And we might have fewer rows of H when the low-stride rows are so small
  // that they fit on a line and can be ignored.

  INT                   Big_Indxs;      // H has this many extra indices
  IMAT*			H;		// the matrix of coeffs of index expr
  LU_FMAT*		H_lu;		// H, LU factored
  LU_FMAT*		H_lu_s;		// H_s, LU factored
  VECTOR_SPACE<FRAC>*	KerH;		// Ker H, or NULL
  VECTOR_SPACE<FRAC>*	OKerH;		// Orthogonal subspace of Ker H, or NULL
};

class RG_ITER : public CHAIN_ITER {
  DECLARE_CHAIN_ITER_CLASS(RG_ITER, RG_NODE, RG_LIST);
};

class RG_CONST_ITER : public CHAIN_ITER {
  DECLARE_CHAIN_CONST_ITER_CLASS(RG_CONST_ITER, RG_NODE, RG_LIST);
};

static void Compute_Stride_One_Loop(const IMAT* h,
                                    INT         elements,
                                    INT         s1row,
                                    INT*        s1loop,
                                    INT*        stride)
{
  *s1loop = -1;
  INT j;
  for (j = 0; j < elements; j++) {
    INT n = (*h)(s1row,j);
    if (n == 0)
      continue;
    INT nn = n < 0 ? -n : n;
    // if (*s1loop != -1 && *stride <= nn)
    //  continue;

    // also, we don't want this element appearing alone in another loop
    INT i;
    for (i = 0; i < s1row; i++) {
      if ((*h)(i,j) != 0) {
        INT jj;
        for (jj = 0; jj < elements; jj++) {
          if (jj != j && (*h)(i,jj) != 0)
            break;
        }
        if (jj == elements)	// appears alone here
          break;
      }
    }

    if (i == s1row) {		// doesn't appear alone anywhere
      *s1loop = j;
      *stride = nn;
    }
  }
}


void Fill_Constant_Array(const ACCESS_ARRAY* aa, INT* c,
                         const INT* loop, INT nloops,
                         INT big_indxs, INT indxs)
{
  Is_True(indxs <= aa->Num_Vec() + big_indxs, ("Broken input"));

  INT b = 0;
  INT i;
  for (i = big_indxs; i < indxs; i++) {
    INT constant = aa->Dim(i-big_indxs)->Const_Offset;
    for (INT j = 0; j < nloops; j++) {
      INT coeff = aa->Dim(i-big_indxs)->Loop_Coeff(loop[j]);
      if (ABS(coeff) > MAX_COEFF) {
        if (ABS(constant) >= ABS(coeff)) {
          c[b++] = constant/coeff;
          constant %= coeff;
        }
        else 
          c[b++] = 0;
      }
    }
    c[i] = constant;
  }

  Is_True(big_indxs == b, ("internal check failed"));
}

RG::RG(MEM_POOL* p, ACCESS_ARRAY* aa, INT nloops,
       const INT* loop, INT element_size, SYMBOL sym, const INT* approx_iters,
       BOOL using_tlb, WN* wn, INT middle_loop_no, INT* approx_inner_iters)
  : Pool(p), Aa(aa), Elements(nloops),
    Rglist(p, nloops, element_size, loop, approx_iters, using_tlb),
    Sym(sym), Big_Indxs(0),
    Is_Local_Array(wn?Is_Local_Array_Reference(wn):TRUE)
{
  Is_True(nloops <= SNL_MAX_LOOPS, ("Too many loops in nest"));

  INT indxs = aa->Num_Vec();

  // TODO OK: Note that probably could make indxs >= 1 be the tests below.
  // When indxs is zero at the end of the loop, then do we crash when indxs==0?
  // Probably, but if we are more clever, we can recognize that this
  // array always uses up exactly one cache line.  Probably not a big deal.

  Rglist.Effective_Element_Size = Rglist.Element_Size;
  if (indxs > 1) {
    if (wn == NULL ||
        Long_Line_Policy == LONG_LINE_OFF ||
        (Long_Line_Policy == LONG_LINE_CACHE_ONLY && using_tlb) ||
        (Long_Line_Policy == LONG_LINE_TLB_ONLY && !using_tlb)) {
      // don't bother optimizing for long cache lines.
    }
    else if (indxs != WN_num_dim(wn)) {
      // TODO OK: a shame to miss this opportunity to optimize for long cache
      // lines, but kind of hard.  This condition may arise as a result of
      // (de)linearization.
    }
    else {
      INT   lsz = (Rglist.Using_TLB ? Cur_Mhd->Page_Size : Cur_Mhd->Line_Size);
      while (indxs > 1) {
	WN* dim_wn = WN_array_dim(wn,indxs-1);
	if (WN_operator(dim_wn) != OPR_INTCONST)
	  break;
	if (Rglist.Effective_Element_Size * WN_const_val(dim_wn) >= lsz)
          break;
	Rglist.Effective_Element_Size *= INT(WN_const_val(dim_wn));
	indxs--;
      }
    }
  }

  // TODO OK for now:  The vector space computations below can produce
  // overflow, especially FRAC -=(), and rather easily if the coefficients
  // are large.  So what we do is this: if a coefficient > 20, then
  // pretend it's its own index.  So a(i+40*j+100*k) -> a(j,k,i).  It's
  // not perfect, but it's pretty good.  So Big_Indxs is the number of
  // big coefficients.
  INT i;
  for (i = 0; i < indxs; i++) {
    INT j;
    for (j = 0; j < nloops; j++) {
      INT coeff = aa->Dim(i)->Loop_Coeff(loop[j]);
      if (ABS(coeff) > MAX_COEFF)
        Big_Indxs++;
    }
  }

  indxs += Big_Indxs;

  H = NULL;
  H_lu = NULL;
  H_lu_s = NULL;
  KerH = NULL;
  OKerH = NULL;
  C = CXX_NEW_ARRAY(INT, indxs, Pool);
  Fill_Constant_Array(aa, C, loop, nloops, Big_Indxs, indxs);
  Rglist.Stride_Loop = -1;
  Rglist.Next_Outer_Stride_Loop = -1;
  Rglist.Actual_Stride = 0;
  for (i = 0; i < SNL_MAX_LOOPS; i++) 
    Rglist.Stride_Loops[i] = -1; 

  if (nloops) {
    H = CXX_NEW(IMAT(indxs, nloops, Pool), Pool);
    H->D_Zero();

    FMAT Hf(indxs, nloops, Pool);
    FMAT Hf_s(indxs, nloops, Pool);

    INT b = 0;
  
    for (i = Big_Indxs; i < indxs; i++) {
      INT j;
      for (j = 0; j < nloops; j++) {
        INT coeff = aa->Dim(i-Big_Indxs)->Loop_Coeff(loop[j]);
        if (ABS(coeff) > MAX_COEFF) {

          // TODO (nenad, 03/23/99):
          // We shoud pass unrolls array from Compute_Footprint
          // and use unrolls[loop[j]] here instead of 1.
          // This "delinearization" of references should also
          // be done when selecting stride-one loops. PV 675685
          // reports poor performance on matrix multiply with
          // linearized references. At least in such a simple
          // case we should be able to do as well as with the
          // nicely written matrix multiply.

          (*H)(b,j) = 1;
          Hf(b,j) = FRAC(1);
          Hf_s(b++,j) = FRAC(1); // if big coeff occurs in stride one row, counts
        }
        else {
          (*H)(i,j) = coeff;
          Hf(i,j) = FRAC(coeff);
          if (i < indxs - 1)
            Hf_s(i,j) = FRAC(coeff);
        }
      }
    }
    Is_True(Big_Indxs == b, ("internal check failed"));
    Is_True(H->Rows() == indxs, ("internal check2 failed"));

    H_lu = CXX_NEW(LU_FMAT(Hf, Pool), Pool);
    H_lu_s = CXX_NEW(LU_FMAT(Hf_s, Pool), Pool);

    KerH = CXX_NEW(VECTOR_SPACE<FRAC>(*H_lu, Pool), Pool);
    KerH->Beautify();
    VECTOR_SPACE<FRAC> KerH_s(*H_lu_s, Pool);
    KerH_s.Beautify();
    OKerH = CXX_NEW(VECTOR_SPACE<FRAC>(KerH->N(), Pool, TRUE), Pool);
    *OKerH -= *KerH;
    OKerH->Beautify();
    FmtAssert(OKerH->Basis().Cols() == nloops, ("OKerH bug"));

    // stride 1 loop

    if (KerH->D() != KerH_s.D()) {
      FmtAssert(KerH->D() == KerH_s.D() - 1,
                ("%d != %d -1", KerH->D(), KerH_s.D()));
      Compute_Stride_One_Loop(H, Elements, indxs-1,
                              &Rglist.Stride_Loop, &Rglist.Actual_Stride);

      if (wn && Rglist.Stride_Loop != -1 && indxs >= 2) {
        INT actual_stride = 0;
        Compute_Stride_One_Loop(H, Elements, indxs-2,
                                &Rglist.Next_Outer_Stride_Loop,
                                &actual_stride);
        if (actual_stride != 1 ||
            Rglist.Next_Outer_Stride_Loop == Rglist.Stride_Loop)
          Rglist.Next_Outer_Stride_Loop = -1;
      }

      // Compute the stridedness of each of the loops. 
      Rglist.Stride_Loops[0] = Rglist.Stride_Loop; 
      Rglist.Stride_Loops[1] = Rglist.Next_Outer_Stride_Loop; 
      if (Rglist.Next_Outer_Stride_Loop != -1) { 
	for (i = 2; i <= indxs; i++) {
	  if (wn != NULL) { 
	    INT actual_stride = 0;
	    Compute_Stride_One_Loop(H, Elements, indxs - (i + 1), 
	      &(Rglist.Stride_Loops[i]), &actual_stride); 
	    if (actual_stride != 1) {
	      Rglist.Stride_Loops[i] = -1; 
	      break; 
	    } 
            INT j;
	    for (j = 0; j < i; j++) 
	      if (Rglist.Stride_Loops[j] == Rglist.Stride_Loops[i])
		break; 
	    if (j < i) {
	      Rglist.Stride_Loops[i] = -1; 
	      break; 
	    } 
	  }
	}
      }
      if (Debug_Cache_Model >= 2) { 
	fprintf(TFile, "Stride Loops = {"); 
        INT i;
        for (i = 0; i < SNL_MAX_LOOPS; i++) { 
	  if (Rglist.Stride_Loops[i] == -1) 
	    break; 
          fprintf(TFile, "%d", Rglist.Stride_Loops[i]); 
	  if (i+1 < SNL_MAX_LOOPS && Rglist.Stride_Loops[i+1] != -1)
	    fprintf(TFile, ","); 
        } 
	fprintf(TFile, "}\n"); 
      } 

      // When leading dimension is a power of two, or near it, it can cause
      // cache problems.  We use a hack to model this: if the leading dimension
      // (threfore, stride one reuse) is near a power of two, and that's also
      // the "middle" loop in the tile, just have that reuse go away.
      // TODO OK: there are better ways to do this.  If we know how many
      // iterations of the "inner" loops, we can determine exactly how much
      // reuse we lose.  But that's complicated (although we so something
      // somewhat similar with "edge effects").  The constant in the formula
      // for f was pulled out of my butt.  TODO: if the loop is blocked
      // further in (approx_inner_iters[loop[Rglist.Stride_Loop]] > 1), then we
      // don't bother.  This is only approximately right.  Better would be
      // to get reuse within the inner block but not outside.

      if (Rglist.Stride_Loop != -1 && wn && !using_tlb) {
        INT s1_depth = loop[Rglist.Stride_Loop];
        if (middle_loop_no == s1_depth && approx_inner_iters[s1_depth] <= 1 &&
            Rglist.Effective_Element_Size < Cur_Mhd->Line_Size &&
            LNO_Power_Of_Two_Hack && Cur_Mhd->Type == MHD_TYPE_CACHE) {
          WN*   wn_leading_dim = WN_array_dim(wn, WN_num_dim(wn)-1);
          if (WN_operator(wn_leading_dim) == OPR_INTCONST) {
            INT64 leading = WN_const_val(wn_leading_dim);
            const INT pulled_out_of_my_butt = 16;
            INT64 f = (INT64(Cur_Mhd->Associativity) * INT64(Cur_Mhd->Size)) 
                       / (Cur_Mhd->Line_Size * pulled_out_of_my_butt);

            // very small bounds, like [2], are not a problem
            if (leading >= f/2) {
              INT   diff = leading % f;
              if (diff > f/2)
                diff = f - diff;

              if ((diff-2)*128 < f) { // within 2 of f, or 3 if f>128, or 4 ...
                Rglist.Actual_Stride = -1;
                Rglist.Stride_Loop = -1;
                if (Debug_Cache_Model >= 3) {
                  fprintf(TFile, "pwr2 hack (spatial locality cs=%lld): ",
                          Cur_Mhd->Size);
                  Print(TFile);
                  fprintf(TFile, "\n");
                }
              }
            }
          }
        }
      }
    }
  }
}

RG::~RG()
{
  if (H)
    CXX_DELETE(H, Pool);
  if (H_lu)
    CXX_DELETE(H_lu, Pool);
  if (H_lu_s)
    CXX_DELETE(H_lu_s, Pool);
  if (KerH)
    CXX_DELETE(KerH, Pool);
  if (OKerH)
    CXX_DELETE(OKerH, Pool);
  if (C)
    CXX_DELETE_ARRAY(C, Pool);
}

RG_NODE::RG_NODE(INT entries, const INT* mn, const INT* mx,
		 INT mn_stride, INT mx_stride)
  : Entries(entries), Mn_Stride(mn_stride), Mx_Stride(mx_stride)
{
  INT i;
  for (i = 0; i < entries; i++) {
    Mn[i] = mn[i];
    Mx[i] = mx[i];
  }
}

void RG_NODE::Print(FILE* f) const
{
  INT i;
  for (i = 0; i < Entries; i++) {
    fprintf(f, "%s%d/%d", (i == 0 ? "[" : " "), Mn[i], Mx[i]);
  }
  fprintf(f, " stride=%d/%d]", Mn_Stride, Mx_Stride);
}


RG_LIST::~RG_LIST()
{
  RG_ITER iter(this);
  RG_NODE* next = NULL;
  for (RG_NODE* rg = iter.First(); !iter.Is_Empty(); rg = next) {
    next = iter.Next();
    CXX_DELETE(rg, Pool);
  }
}

// given an RG_LIST with many RN_NODEs, it may be the case that
// some of the RG_NODES (e.g. if the parameter is true, only the first)
// may need to be combined with others.

void RG_LIST::Simplify(BOOL first_only)
{
  BOOL changed = FALSE;

  RG_ITER iter(this);
  RG_NODE* rgnext = NULL;
  for (RG_NODE* rg = iter.First(); rg && !iter.Is_Empty(); rg = rgnext) {
    rgnext = first_only ? NULL : iter.Next();

    // no ordering, so simply start from rg and combine.

    for (RG_NODE* rg2 = rg->Next(); rg2; rg2 = rg2->Next()) {
      BOOL	ok_to_insert;
      INT       lsz = Using_TLB ? Cur_Mhd->Page_Size : Cur_Mhd->Line_Size;
      INT       esz = Effective_Element_Size;

      ok_to_insert = (esz*(rg->Mn_Stride - rg2->Mx_Stride) < lsz &&
	              esz*(rg2->Mn_Stride - rg->Mx_Stride) < lsz);
      if (ok_to_insert) {
        INT i;
	for (i = 0; i < Elements; i++) {
	  if (rg2->Mn[i] - rg->Mx[i] > Max_Diff[i] ||
	      rg->Mn[i] - rg2->Mx[i] > Max_Diff[i]) {
	    ok_to_insert = FALSE;
	    break;
	  }
	}
      }
      if (ok_to_insert) {
        INT i;
        for (i = 0; i < Elements; i++) {
          rg2->Mn[i] = MIN(rg->Mn[i], rg2->Mn[i]);
          rg2->Mx[i] = MAX(rg->Mx[i], rg2->Mx[i]);
        }
	rg2->Mx_Stride = MAX(rg2->Mx_Stride, rg->Mx_Stride);
	rg2->Mn_Stride = MIN(rg2->Mn_Stride, rg->Mn_Stride);
        Remove(rg);
        CXX_DELETE(rg, Pool);
	changed = TRUE;
	break;
      }
    }
  }

  if (changed)
    Simplify(FALSE);
}

// put on the list.  But if there's already one on the list, then we
// may have to merge.

void RG_LIST::Insert(const INT* c, INT stride, BOOL has_store)
{
  Count++;
  if (has_store)
    Store_Count++;

  switch (Len()) {
   case 0:
    if (Debug_Cache_Model >= 4)
      fprintf(TFile, "INSERT<2>: first in this rglist\n");
    Prepend(CXX_NEW(RG_NODE(Elements, c, c, stride, stride), Pool));
    break;

   case 1:
    {
      RG_NODE*	rg = Head();
      BOOL	ok_to_insert;

      INT	lsz = Using_TLB ? Cur_Mhd->Page_Size : Cur_Mhd->Line_Size;
      INT       esz = Effective_Element_Size;
      double	elements_in_line = double(lsz)/esz;
      if (Stride_Loop != -1 && Actual_Stride < lsz)
        elements_in_line += Max_Diff[Stride_Loop] * Actual_Stride;
      elements_in_line = MAX(elements_in_line, 1);

      ok_to_insert = (stride - rg->Mx_Stride < elements_in_line &&
		    rg->Mn_Stride - stride < elements_in_line);

      if (ok_to_insert) {
        INT i;
        for (i = 0; i < Elements; i++) {
	  if (c[i]-rg->Mx[i] > Max_Diff[i] || rg->Mn[i]-c[i] > Max_Diff[i]) {
	    if (Debug_Cache_Model >= 4) {
	      fprintf(TFile, "INSERT<2>: index clash: can't go in group: ");
              rg->Print(TFile);
            }
	    ok_to_insert = FALSE;
	    break;
	  }
        }
      }
      else {
        if (Debug_Cache_Model >= 4) {
	  fprintf(TFile, "INSERT<2>: cache line clash: can't go in group: ");
          rg->Print(TFile);
        }
      }
  
      if (ok_to_insert) {
        INT i;
        for (i = 0; i < Elements; i++) {
          rg->Mx[i] = MAX(rg->Mx[i], c[i]);
          rg->Mn[i] = MIN(rg->Mn[i], c[i]);
        }
        rg->Mx_Stride = MAX(rg->Mx_Stride, stride);
        rg->Mn_Stride = MIN(rg->Mn_Stride, stride);
        if (Debug_Cache_Model >= 4) {
          fprintf(TFile, "INSERT<2>: inserted, producing ");
          Print(TFile);
          fprintf(TFile, "\n");
        }
      }
      else
        Prepend(CXX_NEW(RG_NODE(Elements, c, c, stride, stride), Pool));
      break;
    
   }
   default:
    if (Debug_Cache_Model >= 4)
      fprintf(TFile, "INSERT<2>: rglist already has %d -- complex\n", Len());
    // important to call insert rather than append, so simplify knows just
    // to simplify the first element.
    Prepend(CXX_NEW(RG_NODE(Elements, c, c, stride, stride), Pool));
    Simplify(TRUE);

    break;
  }
}

// Returns FALSE if there is no solution to Hx = (C_aa - C_representative).
// Otherwise, do the insertion by calling the above insertion routine.

BOOL RG::Insert(const ACCESS_ARRAY* aa, BOOL has_store, INT nloops,
                const INT* loop)
{
  INT  cc[SNL_MAX_LOOPS];
  FRAC x[SNL_MAX_LOOPS];

  INT  indxs = nloops ? H->Rows() : aa->Num_Vec() - 1;

  // Because of (de)linearization, it's not enough to just say that
  // look at Const_Offset for each access vector.
  INT* newC = (INT*) alloca(sizeof(INT)*indxs);
  Fill_Constant_Array(aa, newC, loop, nloops, Big_Indxs, indxs);

  // are all constants are the same except for possibly the stride one dim?
  BOOL zero_const_diff = TRUE;  // except maybe for stride one dimension
  INT i;
  for (i = 0; i < indxs-1; i++) {
    if (newC[i] != C[i]) {
      zero_const_diff = FALSE;
      break;
    }
  }

  // if so, then just insert that along the same stride-one row
  if (zero_const_diff) {
    INT stride1d = newC[indxs-1] - C[indxs-1];
    if (Debug_Cache_Model >= 4)
      fprintf(TFile, "INSERT: Inserting! Stride one diff: %d\n", stride1d);
    INT i;
    for (i = 0; i < Elements; i++)
      cc[i] = 0;
    Rglist.Insert(cc, stride1d, has_store);
    return TRUE;
  }

  BOOL ok = FALSE;
  if (nloops) {
    // not, so we have to solve H(cc) = (newC-C), ignoring the stride-one row.
    FRAC* c = CXX_NEW_ARRAY(FRAC, indxs, &LNO_local_pool);
    for (i = 0; i < indxs-1; i++)
      c[i] = FRAC(newC[i] - C[i]);
    for ( ; i < indxs; i++)
      c[i] = FRAC(0);

    ok = H_lu_s->Particular_Solution(c, x);
    CXX_DELETE_ARRAY(c, &LNO_local_pool);
  }

  // see if the particular solution (H_lu_s, ignoring stride one dimension)
  // is integral and otherwise valid.
  if (ok) {
    for (i = 0; i < Elements; i++) {
      if (x[i].D() != 1) {
        ok = FALSE;
        break;
      }
    }
  }

  // if valid, then insert the cc vector and stride one difference.
  if (ok) {
    INT stride1d = newC[indxs-1] - C[indxs-1];

    for (i = 0; i < Elements; i++)
      cc[i] = x[i].N();

    // This shouldn't really matter, but it's in our definition: if there
    // is a stride one loop, it's mx and mn will always be zero.
    INT lsz = (Rglist.Using_TLB ? Cur_Mhd->Page_Size : Cur_Mhd->Line_Size);
    if (Rglist.Stride_Loop != -1 && lsz > Rglist.Effective_Element_Size)
      cc[Rglist.Stride_Loop] = 0;

    if (Debug_Cache_Model >= 4) {
      fprintf(TFile, "INSERT: Particular solution:");
      INT i;
      for (i = 0; i < Elements; i++)
        fprintf(TFile, " %d", cc[i]);
      fprintf(TFile, " with const diff %d\n", stride1d);
    }

    Rglist.Insert(cc, stride1d, has_store);
    return TRUE;
  }

  // doesn't belong in this uniformly generated set.  E.g. a(i,2j) doesn't
  // belong in a(i,2j+1), because they touch totally different data.
  if (Debug_Cache_Model >= 4)
    fprintf(TFile, "INSERT: No particular solution!  No insertion!\n");

  return FALSE;
}

void RG_LIST::Print(FILE* f) const
{
  fprintf(f, "<es=%d, s1l=%d, s1r=%d, cnt=%d(w=%d) tlb=%d>",
	  Effective_Element_Size, Stride_Loop, Actual_Stride,
          Count, Store_Count, Using_TLB);
  RG_CONST_ITER iter(this);
  for (const RG_NODE* rg = iter.First(); !iter.Is_Empty(); rg = iter.Next()) {
    fprintf(f, " ");
    rg->Print(f);
  }
}

void RG::Print(FILE* f) const
{
  fprintf(f, "RG_LIST: %s", Sym.Name());
  if (Is_Local_Array)
    fprintf(f, "<local>");
  Aa->Print(f);
  fprintf(f, "{");
  Rglist.Print(f);
  fprintf(f, "}\n");

}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//
// Compute the footprint for
//
//	for i1 ... in
//	  code
//
// The return value is a formula for the footprint and also the "d" value
// for loop i1.
//
// This is done by taken the array reference list (from the loop model)
// as input.
//
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// Formula_For_Nk:
//   This is the formulaic representation of Nk.  When outersz is -1,
//   the formula is Var(k).  Otherwise, it's Var(k-1) except when k==0,
//   because then outersz is the number of iterations of the outer loop.
//   Two nuances: (1) approx_inner_iters[all_loops[k]] tells us how much
//   tiling has already been done inside, e.g. we are tiling for L2 and
//   we've already tiled for L1.   In that case, we multiply by the
//   iters inside.  If a loop had already been blocked 10, then 10*Var(k-1)
//   would be the number of iterations we actually execute by blocking
//   for L2 only Var(k-1).  (2) If const_answer is NULL, we compute the
//   footprint.  But if it's not, then whenever the answer is constant
//   (really, only if k==0 and outersz != -1), then it returns the answer
//   there instead and returns NULL.  Finally, if k >= nloops, this was a
//   variable that was blocked inside (by approx_inner_iters[all_loops[k]])
//   but is not in this block: don't use vk, but just use 1.

FORMULA* Formula_For_Nk(INT        k,
                        INT        v_first,
                        INT64      outersz,
                        INT*       approx_inner_iters,
                        INT        nloops,
                        const INT* all_loops,
			INT*       const_answer)
{
  FmtAssert(outersz == -1 || outersz > 0, ("Bad outersz %lld", outersz));
  
  INT       innersz = MAX(approx_inner_iters[all_loops[k]],1);
  FORMULA*  f = NULL;

  if (k == 0 && v_first == -1) {
    if (const_answer)
      *const_answer = outersz * innersz;
    else
      f = FORMULA::Const(outersz * innersz);
  }
  else if (k >= nloops) {
    if (const_answer)
      *const_answer = innersz;
    else
      f = FORMULA::Const(innersz);
  }
  else {
    f = FORMULA::Var(v_first + k);
    if (innersz > 1)
      f = FORMULA::Mul(f, FORMULA::Const(innersz));
  }

  return f;
}

static INT Cvt_To_All_K(INT loop, INT all_nloops, const INT* all_loops)
{
  INT kk;
  for (kk = 0; kk < all_nloops; kk++) {
    if (loop == all_loops[kk])
      return kk;
  }
  FmtAssert(0, ("can't find loop=%d in all_loops", loop));
  return -1;
}

/** 
*** Please see the comments at top of this file for a discussion of this
*** simple and important routine.  Note in particular the todo in that
*** section: X is guessed to be 1 instead of 3 for a(3i), a(3i+3) when there
*** is a short cache line.  That means that it thinks we are wasting cache
*** in that case when, in reality, the cache line is so short that we are
*** not.  Simple fixes for that are a disaster in the common case.  We must
*** realize that when we have a(3i), a(3i+1), a(3i+2) that we use 3 times
*** the range of i (x=3), so that we get the right answer for register
*** unrolling.
***
***	A_k = {N_k + Max_k - Min_k,             for k, non-stride1
***	      {(N_j-1)*X + cls + Max_j - Min_j  for j, stride1
***
*** This formula is good, but for multiple level blocking there is
*** a difference: N_k is to be replaced by v_k normally, but by
*** B_k*v_k when there's blocking, so that v = 10 -> ten iterations
*** of the outer stripped loop.  Also, for the loops in all_loops
*** but not loops, we know how many iterations they have, so just
*** plug it in.  This is what Formula_For_Ak hides.
***
*** k:              which loop.  0 is the middle loop.  all_loops[k]
***                 holds the depth (loop number).  That is, k is an
***                 index into all_loops.
*** v_first:        the outermost loop should map in the formula into
***                 v_first, the next into v_first+1, etc.
*** outersz:        if v_first is -1, and we are dealing with the
***                 outermost loop, then clearly we shouldn't use v_-1.
***                 Instead use the value outersz.  This value is ignored
***                 in all other cases.
*** n:              The Reference group we are determining A_k for.
*** rg:             The RG that contains n
*** inner_sz:       arl_stripsz for this loop, or if that's 0, est_iters.
*** outer_sz:       If positive, use this value as number of iters.
***                 Note that outer_sz*inner_sz*unrolls is number of
***                 iterations of orig variable, roughly.
***                 If 0, use v_(k)
***                 If -1, use v_(k-1).
*** element_size:   bytes
*** coeff:          Only used if k is the stride one loop.
***                 Elements used per cache line.  Thus non-zero.
***                 Absolute value taken, so negative okay.
*** using_tlb:      TRUE if we are modelling the TLB.
*** inner_iters_to_stop_edge_effects: if >= 1, then generate a test
***                 that removes edge effects when the number of iterations
***                 is at least this value, so long as the next outer loop
***                 is also in the nest.  For example, suppose we have
***                         do i = 1, N
***                           do j = 1, N
***                             a(i,j)
***                 If i is the middle loop, then use 1 for this value ...
***                 because we don't strip the middle loop.  Otherwise,
***                 make sure j is in the nest.  Remember, if there are
***                 v iterations of i, then the number of words used in
***                 a line is not v+3, say, but v.
***                 Now consider
***                         do j = 1, N
***                           do i = 1, N
***                             a(i,j)
***                 Same idea.
***                 The value "0" means "ignore edge effects".  In that case
***                 "unrolls" irrelevant.
*** est_iters2:     estimated iters for the edge effect loop.  Used only when
***                 inner_iters_to_stop_edge_effects is zero.
*** unrolls:        Used to help compute edge effects.  If the next outer
***                 loop is unrolled, edge effects can be reduced by that
***                 factor.
**/

static FORMULA* Formula_For_Ak(INT        k,
                               INT        v_first,
			       INT64      outersz,
                               INT*       approx_inner_iters,
                               INT        nloops,
			       INT        all_nloops,
                               const INT* all_loops,
                               const INT* nc_loops,
                               RG_NODE*   n,
                               RG*        rg,
                               INT        coeff,
                               BOOL       using_tlb,
                               INT        inner_iters_to_stop_edge_effects,
			       const INT* unrolls)
{
  Is_True(k >= 0 && k < n->Entries, ("Indexed RG_NODE out of bounds"));
  INT      lsz = using_tlb ? Cur_Mhd->Page_Size : Cur_Mhd->Line_Size;
  INT      esz = rg->Rglist.Effective_Element_Size;
  INT      stride = rg->Rglist.Actual_Stride;

  // is the loop a stride one loop or not.  From the odd discussion above,
  // let's say the line size is 2 elts.  Let's say we have a(4*i).  Then no,
  // even though technically i is the stride one loop.  But if we have
  // a(4*i) through a(4*i+3) then we must use the stride one formulas.

  INT      stride_loop = rg->Rglist.Stride_Loop;
  if (stride_loop != -1)
    stride_loop = Cvt_To_All_K(nc_loops[stride_loop], all_nloops, all_loops);
  if (stride_loop != k ||
      lsz <= (stride-(n->Mx_Stride-n->Mn_Stride))*esz) {
    // for non-stride loops, just add in mx-mn -- see derivation at top

    FORMULA* f = Formula_For_Nk(k, v_first, outersz, approx_inner_iters,
                                nloops, all_loops, NULL);
    INT diff = n->Mx[k] - n->Mn[k];
    if (diff < 0) {
      Is_True(0, ("Impossible diff"));
      diff = 0;
    }
    if (diff)
      f = FORMULA::Add(f, FORMULA::Const(diff));

    // Add in a factor to compensate for the stridedness of non-stride-one
    // references. 
    INT i;
    for (i = 0; i < SNL_MAX_LOOPS && rg->Rglist.Stride_Loops[i] != -1; i++) 
      if (rg->Rglist.Stride_Loops[i] == k)
	break; 
    FORMULA* fx = NULL; 
    if (i < SNL_MAX_LOOPS && rg->Rglist.Stride_Loops[i] != -1) { 
      INT cls = (esz < lsz) ? lsz/esz : 1;
      double d = double(cls - 1); 
      fx = FORMULA::Const(d); 
      INT j;
      for (j = 0; j < i; j++) {
	FORMULA* fxp = Formula_For_Nk(rg->Rglist.Stride_Loops[j], v_first, 
	  outersz, approx_inner_iters, nloops, all_loops, NULL);
        if (unrolls[j] != 1)
          fx = FORMULA::Div(fx, FORMULA::Mul(FORMULA::Const(unrolls[j]), fxp));
	else 
          fx = FORMULA::Div(fx, fxp); 
      } 
    } 
    if (fx != NULL) 
      f = FORMULA::Add(f, fx); 

    if (Debug_Cache_Model >= 3) {
      fprintf(TFile, "Ak <1,k=%d> returning ", k);
      f->Print(TFile);
      fprintf(TFile, "\n");
    }
    return f;
  }

  // First decide if there is a next inner loop.  See discussion at top
  // of file.  But, basically, if the stride one loop executes all its
  // iterations, and the next lowest stride loop executes as well, then
  // the cache lines fetched at the edge of the iterations are not wasted.

  INT      next_inner_k = rg->Rglist.Next_Outer_Stride_Loop;
  if (next_inner_k != -1)
    next_inner_k = Cvt_To_All_K(nc_loops[next_inner_k], all_nloops, all_loops);
  INT      cls = (esz < lsz) ? lsz/esz : 1;
  if (coeff < 0)
    coeff = -coeff;
  // the stride loop: x = min(abs(coeff), cls+spread), which is a slightly
  // strange formula: see comments, since it's important.
  INT x = MIN(coeff, cls + n->Mx_Stride - n->Mn_Stride);
  x = MAX(x, 1);

  BOOL   reduce_edge_effects;
  
  if (LNO_Cache_Model_Edge_Effects == FALSE || cls == 1)
    reduce_edge_effects = FALSE;
  else if (inner_iters_to_stop_edge_effects == 0)
    reduce_edge_effects = TRUE;
  else if (next_inner_k == -1 || inner_iters_to_stop_edge_effects < 0)
    reduce_edge_effects = FALSE;
  else
    reduce_edge_effects = TRUE;
    
  // The formula is
  //          ((f-1)*x + 1)     [one elt for the first iter, x for each addtl.]
  //                            [because x is the stride]
  //          + max-min         [e.g. 3 in a(2*i+3) and a(2*i)]
  //          + cls-1           [edge effects]
  // Note that with edge effects on we actually have to divide by uu, the
  // unroll factor of the "next_inner_k" loop, the loop in which these
  // edge effects can be taken advantage of.  Likewise, we'd also need
  // to divide that quantity by v, the number of iterations that execute
  // in the loop that reduces edge effects for long stride loops, est_iters2.
  // Thus the full general formula is
  //          f*x + 1 - x + max - min + (cls-1)/(uu*v)

  FORMULA* f = Formula_For_Nk(k, v_first, outersz, approx_inner_iters,
                              nloops, all_loops, NULL);
  INT      uu = next_inner_k == -1 ? 1 : unrolls[next_inner_k];
  FORMULA* v = NULL;                // meaning 1, which we optimize away
  if (reduce_edge_effects && inner_iters_to_stop_edge_effects && cls > 1) {
    FORMULA* fdup = f->Duplicate();
    v = Formula_For_Nk(next_inner_k, v_first, outersz,
                       approx_inner_iters, nloops, all_loops, NULL);
    if (uu > 1)
      v = FORMULA::Mul(FORMULA::Const(uu), v);
    if (reduce_edge_effects > 0)
      v = FORMULA::Cond(
        FORMULA::Lt(fdup, FORMULA::Const(inner_iters_to_stop_edge_effects)),
        FORMULA::Const(uu),
        v);
  }

  FORMULA* fx = x == 1 ? f : FORMULA::Mul(f, FORMULA::Const(x));
  FORMULA* ans;

  if (v == NULL) {
    double d = 1 - x + n->Mx_Stride - n->Mn_Stride + double(cls - 1)/uu;
    ans = fabs(d) < 0.000001 ? fx : FORMULA::Add(fx, FORMULA::Const(d));
  }
  else {
    INT d = 1 - x + n->Mx_Stride - n->Mn_Stride;
    ans = FORMULA::Add(d == 0 ? fx : FORMULA::Add(fx, FORMULA::Const(d)),
                       FORMULA::Div(FORMULA::Const(cls-1), v));
  }
  if (Debug_Cache_Model >= 3) {
    fprintf(TFile, "Ak <2,k=%d> returning ", k);
    ans->Print(TFile);
    fprintf(TFile, "\n");
  }
  return ans;
}

FORMULA* COMPUTE_FOOTPRINT_RVAL::AllFormula()
{
  if (_formula == NULL) {
    if (RFormula == NULL)
      _formula = WFormula;
    else if (WFormula == NULL)
      _formula = RFormula;
    else
      _formula = FORMULA::Add(RFormula, WFormula);
  }
  return _formula;
}

void COMPUTE_FOOTPRINT_RVAL::Print(FILE* f) const
{
  fprintf(f, "Footprint D=%d,", D);
  fprintf(f, " rformula=");
  if (RFormula)
    RFormula->Print(f);
  else
    fprintf(f, "<none>");
  fprintf(f, " wformula=");
  if (WFormula)
    WFormula->Print(f);
  else
    fprintf(f, "<none>");
  fprintf(f, "\n");
}

// Middle_Loop_Power_Of_Two_Hack
// 
// For group locality.
//
// return FALSE when self-interference may be a problem.  There's a good
// chance when the leading dimension is close to a power of two or multiple
// of that.  Suppose we have a(i,j) and the leading dimension is problematic.
// We don't lose reuse in the leading dimension if we have a(i+1,j), but if
// we have a(i,j+1) we might.  We will only if the j loop is inside the i
// loop or if approx_iters_inside[i] > 1.
//
// We say there's a problem if the leading dimension is within 1+1.5%
// of a multiple of assoc*cs*eltsz/8192.  Of course, I've pulled the 8192
// out of my butt.  It might be better to divide by less to make this
// hack happen less often, but it is a fairly conservative things to do,
// making stride one count more inner.

static BOOL Middle_Loop_Pwr2_Group_Hack(INT*         approx_inner_iters,
                                        RG*          rg,
                                        const ARRAY_REF_NODE* n,
                                        BOOL         tlb,
                                        const INT*   nc_loops,
                                        INT          nc_nloops)
{
  if (LNO_Power_Of_Two_Hack == FALSE ||
      tlb ||
      Cur_Mhd->Type != MHD_TYPE_CACHE)
    // TODO OK: at one point, we said  "|| rg->Is_Local_Array", because
    // this was meant to be a cross-interference hack.  With it now
    // more of a self-interference hack, we no longer need this.
    return TRUE;

  WN*   wn = n->Wn;

  if (wn == NULL)
    return TRUE;

  const INT pulled_out_of_my_butt = 16;
  INT64 f = (INT64(Cur_Mhd->Associativity) * INT64(Cur_Mhd->Size)) 
             / (Cur_Mhd->Line_Size * pulled_out_of_my_butt);

  INT64 leading = 1;
  INT   leading_dim = WN_num_dim(wn) - 1;
  INT dim;
  for (dim = leading_dim; dim >= 0; dim--) {
    WN*   wn_dim = WN_array_dim(wn, dim);
    if (WN_operator(wn_dim) != OPR_INTCONST)
      return TRUE;
    leading *= (INT64) WN_const_val(wn_dim);

    // very small bounds, like [2], are not a problem
    if (leading > f/2) {
      INT   diff = leading % f;
      if (diff > f/2)
        diff = f - diff;

      // is diff within 1+1.5% of f?

      if ((diff-2)*128 < f)
        break;
    }
  }

  // We've reached here only if the loops from 0 to dim are problematic.
  // If there's a constant diff in 0 to dim-1, and we can pin a loop
  // to it, then if one of the other problematic dims varies in any loop
  // inside the culprit loop, then we have a problem, or if approx_inner
  // iters of one of those varying loops is >1, a problem.

  ACCESS_ARRAY* aa = n->Array;
  ACCESS_ARRAY* rgaa = rg->Aa;

  if (aa->Num_Vec() != rgaa->Num_Vec())
    return TRUE;

  if (aa->Num_Vec() <= dim)
    return TRUE;
  INT i;
  for (i = 0; i < dim; i++) {
    ACCESS_VECTOR* av = aa->Dim(i);
    ACCESS_VECTOR* rgav = rgaa->Dim(i);
    if (av->Nest_Depth() != rgav->Nest_Depth())
      continue;
    if (av->Const_Offset == rgav->Const_Offset)
      continue;

    // i is the index expression dimension that's broken.
    // Look for a loop to blame.  That'll be status.

    INT status = -1;  // -1: seen no loop; >=0: saw only this; -2 bad
    INT j;
    for (j = 0; j < av->Nest_Depth(); j++) {
      if (av->Loop_Coeff(j) != 0) {
        if (status == -1)
          status = j;
        else
          status = -2;
      }
    }
    if (status < 0)
      continue;

    // Find ncn such that nc_loops[ncn] = j;
    
    BOOL ncn_valid = FALSE;
    INT ncn;
    for (ncn = 0; ncn < nc_nloops; ncn++) {
      if (nc_loops[ncn] == status) {
        ncn_valid = TRUE;
        break;
      }
    }
    if (ncn_valid == FALSE)
      continue;

    // other loops in the nest might defeat reuse.
    INT ncnn; 
    for (ncnn = 0; ncnn < nc_nloops; ncnn++) {
      if (ncnn == ncn)
        continue;

      INT jn = nc_loops[ncnn];
      
      // is this loop used in some other problematic loop?
      INT d;
      for (d = 0; d <= dim; d++) {
        if (d != i && aa->Dim(d)->Loop_Coeff(jn))
          break;
      }

      if (d <= dim) {       // jn is used in a problematic loop.
                            // if jn inside j or blocked, reuse trashed.
        if (ncn < ncnn || approx_inner_iters[jn] > 1) {
          if (Debug_Cache_Model >= 3) {
            fprintf(TFile, "pwr2 hack group locality, cs=%lld: won't insert ",
                    Cur_Mhd->Size);
            n->Print(TFile);
            fprintf(TFile, "into ");
            rg->Print(TFile);
           }
          return FALSE;
        }
      }
    }
  }

  return TRUE;
}

// Cache_Line_Reuse() is a slightly wierd function.  That's because it's
// a general routine that is just used in one specific place.  Probably,
// we should remove the Next_Outer_Stride_Loop code and just use this in
// all cases.  What this does is look at a loop number and an access array.
// If the loop does not appear in any position but the last two of the
// access array, and with a coeff of -1, 0, or 1 in those places, then
// it returns TRUE.  The purpose is so that we can pass in a non-stride
// one loop and ask if the stride one loop is innermost and this next loop
// is next innermost, return TRUE if we use cache lines efficiently at the
// edges (e.g do i,j a(j,i)) or not (e.g. do i,j a(j,k,i)).
// u is how much the loop is unrolled, so we know how large the
// coeff can be.

static BOOL Cache_Line_Edge_Reuse(INT loop, INT u, ACCESS_ARRAY* aa)
{
  FmtAssert(aa, ("Bad access array passed to Cache_Line_Edge_Reuse"));
  INT indxs = aa->Num_Vec();
  INT j;
  for (j = indxs - 1; j >= 0; j--) {
    ACCESS_VECTOR* av = aa->Dim(j);
    INT coeff = av->Loop_Coeff(loop);
    if (coeff > u || (j < indxs-2 && coeff))
      return FALSE;
  }
  return TRUE;
}

// Compute_Footprint:
//   arl:               the usual
//   loop[i]:           loop number of the loop in the tile.
//                      loop[0] is the "middle loop"; loop
//                      loop[nloops-1] is the "innermost"
//   nloops:            loops in the tile.
//   arl_stripsz:       the usual
//   unrolls:           the usual
//   est_iters:         the usual
//   max_iters:         the usual
//   depth:             arl_stripsz[0..depth]
//   stripdepth:        the usual
//   permute_order:     the usual
//   v_first            the outermost loop should map in the formula into
//                      v_first, the next into v_first+1, etc.
//   outersz:           if v_first is -1, and we are dealing with the
//                      outermost loop, then clearly we shouldn't use v_-1.
//                      Instead use the value outersz.  This value is ignored
//                      in all other cases.
//   tlb                whether we are computing the footprint for
//                      cache lines (false) or pages (true).
//   middle_loop_no:    used for stride power of two hack.  Look it up in
//                      RG::RG.  TODO: see group power of two hack, because
//                      that looks like a more effective style: lose the
//                      spatial if any loops inside overrun your data.
//
// return value:
//
//   The return value is a formula that gives the footprint
//   (estimated number of bytes of cache used) by the entire tile.
//   This is parameterized, since the formula has variables (the blocksizes
//   are unknown).  For example, if v_first==0, then we'd get
//   the footprint for a v0 x v1 block, where v0 was the iterations
//   for loop[0] and v1 for loop[1].  That's why it's a formula rather
//   than just a value: because the tile sizes v0 and v1 are unknown.
//
//   In more detail, we need a formula for the number of distinct bytes
//   (if 1 byte of a cache line is used, the whole line is used)
//   used by all iterations of all loops in loop[i].  Suppose
//   arl_stripsz[] has elements greater than 1.  If one of
//   them is a v variable, then instead we want to use (v*arl_stripsz[]),
//   so that a blocksize of 10 might really mean 200, if arl_stripsz were 20.
//   That means we do 10 iterations of this next outer loop.  Now suppose
//   that arl_stripsz is >1 for some loop not in v0, v1, ... .
//
//   See comments in discussion at top of file about handline non_const_loops.

extern COMPUTE_FOOTPRINT_RVAL Compute_Footprint(
                    const ARRAY_REF*  arl,
		    INT               nloops,   // to be tiled
		    const INT*        loops,    // to be tiled
                    const INT*        arl_stripsz,
                    const INT64*      est_iters,
                    const INT64*      max_iters,
                    const INT*        unrolls,
                    INT               depth,
                    INT               stripdepth,
                    const INT*        permute_order,
                    INT               v_first,
		    INT64             outersz,
                    BOOL              using_tlb,
                    INT               middle_loop_no)
{
  COMPUTE_FOOTPRINT_RVAL rval;

  if (Debug_Cache_Model >= 3) {
    fprintf(TFile,
            "Compute_Footprint <tlb=%d>: v_first=%d outersz=%lld loops=(",
            using_tlb, v_first, outersz);
    INT i;
    for (i = 0; i < nloops; i++)
      fprintf(TFile, "%s%d", (i == 0 ? "" : ","), loops[i]);
    fprintf(TFile, ")\n");
  }

  // because of multi-level blocking, there may be more than just nloops in
  // tile tiles.

  INT  all_nloops = nloops;
  INT* all_loops = (INT*) alloca(sizeof(INT)*(depth+1));
  INT i;
  for (i = 0; i < nloops; i++)
    all_loops[i] = loops[i];
  for (i = stripdepth; i <= depth; i++) {
    INT ii;
    for (ii = 0; ii < all_nloops; ii++) {
      if (all_loops[ii] == permute_order[i])
        break;
    }
    if (ii == all_nloops)
      all_loops[all_nloops++] = permute_order[i];
  }

  INT* max_diff = (INT*)alloca(sizeof(INT) * (depth+1));
  for (i = 0; i <= depth; i++)
    max_diff[i] = MAX(5, est_iters[i]-5) * unrolls[i];

  // Approx inner tiles is the iters in the innermost loop, for each loop.
  // That's arl_stripsz[i] if > 1.  

  INT* approx_inner_iters = (INT*)alloca(sizeof(INT) * (depth+1));
  for (i = 0; i <= depth; i++) {
    // initializing all values, but only the values for middle loop and
    // outside will survive
    approx_inner_iters[i] = (arl_stripsz[i] > 1) ? arl_stripsz[i] : 1;
  }
  for (i = 1; i < all_nloops; i++) {
    // overwrite values inside the middle loop, if "full block"
    INT lp = all_loops[i];
    if (arl_stripsz[lp] < 1)
      approx_inner_iters[lp] = est_iters[lp];
  }

  // arl holds a list of (list of references with same base)
  INT ix;
  for (ix = 0; ix < arl->Elements(); ix++) {

    const ARRAY_REF_LIST*      arlist = arl->Array_Ref_List(ix);

    // for this array base, what's the nc_depth
    INT       ncdepth = 0;
    ARRAY_REF_CONST_ITER       iternn(arlist);
    for (const ARRAY_REF_NODE* nnn = iternn.First();
         !iternn.Is_Empty(); nnn = iternn.Next()) {
      if (nnn->Array) {
        INT this_ref_ncdepth = nnn->Array->Non_Const_Loops();
        ncdepth = MAX(ncdepth, this_ref_ncdepth);
      }
    }

    INT  nc_nloops = 0;
    INT* nc_loops = (INT*) alloca(sizeof(INT)*(depth+1));
    INT i;
    for (i = 0; i < all_nloops; i++) {
      if (all_loops[i] >= ncdepth)
        nc_loops[nc_nloops++] = all_loops[i];
    }

    STACK<RG*> rg_stack(&LNO_local_pool);

    ARRAY_REF_CONST_ITER       iter(arlist);

    for (const ARRAY_REF_NODE* n = iter.First();
         !iter.Is_Empty(); n = iter.Next()) {
      // TODO OK: conservatively putting things of different sizes onto
      // different lists.  Generally the right thing (e.g. for common
      // block elements).
      INT esz = n->Element_Size();
#ifdef KEY
      // Bug 6273 - when calculating footprint of an MMILOAD or MISTORE,
      // use the appropriate element size. 
      if (esz == 0)
	esz = WN_element_size(n->Wn);
      // Bug 10708: element size should be positive
      if (esz < 0) esz = -esz;
#endif
      if (Debug_Cache_Model >= 4)
        fprintf(TFile, "-> arl entry from base %d, insertion info\n", ix);

      BOOL inserted = FALSE;
      INT i;
      for (i = 0; i < rg_stack.Elements(); i++) {
        RG* rg = rg_stack.Bottom_nth(i);
        if (esz == rg->Rglist.Element_Size &&
            Same_Ug(n->Array, rg->Aa) &&
            Middle_Loop_Pwr2_Group_Hack(approx_inner_iters, rg, n,
                                        using_tlb, nc_loops, nc_nloops) &&
            rg->Insert(n->Array, n->Has_Store(), nc_nloops, nc_loops)) {
          inserted = TRUE;
          break;
        }
      }
      if (!inserted) {
	RG *rg = CXX_NEW(RG(&LNO_local_pool, n->Array,
                                 nc_nloops, nc_loops, esz,
                                 *arl->Array_Ref_List(ix)->Base_Array,
                                 max_diff, using_tlb, n->Wn, middle_loop_no,
                                 approx_inner_iters),&LNO_local_pool);
        rg_stack.Push(rg);
        rg_stack.Top()->Insert(n->Array, n->Has_Store(), nc_nloops, nc_loops);
      }
    }

    // all elements inserted.  Now compute the footprint.

    for (i = 0; i < rg_stack.Elements(); i++) {

      // compute the footprint for one uniformly generated set

      RG*       rg = rg_stack.Bottom_nth(i);
      RG_LIST*  rglist = &rg->Rglist;
      RG_ITER   iter(rglist);
      INT       esz = rglist->Effective_Element_Size;
      INT       llsz = using_tlb ? Cur_Mhd->Page_Size : Cur_Mhd->Line_Size;
      INT       lsz = MAX(llsz, esz);

      INT       inner_iters_to_stop_edge_effects = -1;
      if (rg->Rglist.Stride_Loop != -1 &&
          rg->Rglist.Next_Outer_Stride_Loop != -1) {
	INT     lp = nc_loops[rg->Rglist.Stride_Loop];
        INT     mest = max_iters[lp];
        if (mest != UNBOUNDED_ITERS)
          inner_iters_to_stop_edge_effects = mest;
      }
      if (inner_iters_to_stop_edge_effects == -1 && nloops == 1) {
        // sometimes we have do i,j a(j,i) where there is no blocking.
        // We want to model so that the edges are properly handled.
        // This means that the loop just outside this loop here allows
        // for edge reuse.
        INT p0;
        for (p0 = depth; p0 >= 0; p0--)
          if (permute_order[p0] == loops[0])
            break;
        if (p0 >= 1) {
          INT loop2 = permute_order[p0-1]; // the loop just outside
          if (Cache_Line_Edge_Reuse(loop2, unrolls[loop2], rg->Aa)) {
            inner_iters_to_stop_edge_effects = 0;
          }
        }
      }

      const FMAT* basis = rg->OKerH ? &rg->OKerH->Basis() : NULL;
 
      for (RG_NODE* n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {

        // compute the footprint for one reference group
        FORMULA* f = NULL;

        if (basis) {
          // compute the new D
          INT   dmultiple = approx_inner_iters[all_loops[0]];
          INT   newd;

          if (rglist->Stride_Loop == 0 && llsz > rglist->Actual_Stride * esz) {
            // middle loop is stride one
            FmtAssert(rglist->Actual_Stride >= 1,
                      ("Bad stride %d for s1loop %d",
                       rglist->Actual_Stride, rglist->Stride_Loop));
            newd = Divceil(n->Mx_Stride - n->Mn_Stride+lsz/esz,
                           rglist->Actual_Stride);
          }
          else
            newd = n->Mx[0] - n->Mn[0];
          if (dmultiple > 1)
            newd = Divceil(newd, dmultiple);
          rval.D = MAX(rval.D, newd);

          /**
          *** Let P = Prod (cij)
          ***       cij != 0
          ***
          ***	   prod          ((         sum     [ P/|c_ik| * A_k ]    )-P)
          ***   OKerH basis vector i      basis components c_ik
          ***     [as long as all |c_ik| <= 16, otherwise, pretend e1, e2, ...]
          ***	[and I don't subtract P if only one term in the product]
          ***
          *** where
          ***
          ***	A_k = {N_k + Max_k - Min_k,             for k, non-stride1
          ***	      {(N_j-1)*X + cls + Max_j - Min_j  for j, stride1
          ***
          *** This formula is good, but for multiple level blocking there is
          *** a difference: N_k is to be replaced by v_k normally, but by
          *** B_k*v_k when there's blocking, so that v = 10 -> ten iterations
          *** of the outer stripped loop.  Also, for the loops in all_loops
          *** but not loops, we know how many iterations they have, so just
          *** plug it in.  This is what Formula_For_Ak hides.
          **/

          // are all constants le the happy coefficient?  (Happy Coefficient
          // is the max coefficient for which we trust our analysis.  Because
          // of unrolling it can be quite large.)

          UINT64 gtmax = 0;
	  INT ii;
          for (ii = 0; ii < basis->Rows(); ii++)
            for (INT jj = 0; jj < basis->Cols(); jj++)
              if ((*basis)(ii,jj).Abs() > Happy_Coefficient)
                gtmax |= (1<<jj);

          INT s1row = rg->H->Rows() - 1;

          if (gtmax) {
            // f is the product of A_k for the too-big k
            INT k;
            for (k = 0; k < basis->Cols(); k++) {
              if (gtmax & (1<<k)) {
	        INT kk = Cvt_To_All_K(nc_loops[k], all_nloops, all_loops);
                FORMULA* fk = Formula_For_Ak(kk, v_first,
                                             outersz, approx_inner_iters,
                                             nloops, all_nloops, all_loops,
					      nc_loops,
					     n, rg,
                                             (*rg->H)(s1row,k), using_tlb,
                                             inner_iters_to_stop_edge_effects,
                                             unrolls);
                f = f ? FORMULA::Mul(f, fk) : fk;
              }
            }
          }

          for (ii = 0; ii < basis->Rows(); ii++) {
            INT p = 1;
	    INT k;	
            for (k = 0; k < basis->Cols(); k++)
              if (!(gtmax & (1<<k)) && (*basis)(ii,k) != 0)
                p = p * (*basis)(ii,k).N();
            p = (p < 0) ? -p : p;

            FORMULA* f2 = NULL;

            for (k = 0; k < basis->Cols(); k++) {
              if (gtmax & (1<<k))
                continue;

              INT c = (*basis)(ii,k).N();
              // this loop is not involved if coeff is 0 and array ref solid
              if (c == 0)
                continue;
	      INT kk = Cvt_To_All_K(nc_loops[k], all_nloops, all_loops);
              FORMULA* fk = Formula_For_Ak(kk, v_first,
                                           outersz, approx_inner_iters,
                                           nloops, all_nloops, all_loops, 
					    nc_loops,
					   n, rg,
                                           (*rg->H)(s1row,k), using_tlb,
                                           inner_iters_to_stop_edge_effects,
                                            unrolls);
              if (p != c)
                fk = FORMULA::Mul(fk, FORMULA::Const(p/ABS(c)));
              f2 = f2 ? FORMULA::Add(f2, fk) : fk;
            }
            if (f2) {
              f = f ? FORMULA::Mul(f, f2) : f2;
            }
          }
        }

        if (f == NULL)
          f = FORMULA::Const(1);
        INT ii; 
        for (ii = 0; ii < nloops; ii++) {
          BOOL found = FALSE;
          for (INT jj = 0; jj < nc_nloops; jj++) {
            if (all_loops[ii] == nc_loops[jj])
              found = TRUE;
          }
          if (found == FALSE) {
            FORMULA* nk = Formula_For_Nk(ii, v_first,
                                         outersz, approx_inner_iters,
                                         nloops, all_loops, NULL);
            f = FORMULA::Mul(nk, f);
          }
        }

        double es_const = esz;
        if ((rg->Rglist.Stride_Loop == -1 || lsz <= esz) &&
            !arlist->Is_Scalar_Expanded()) {
          es_const *= double(lsz)/esz + n->Mx_Stride - n->Mn_Stride;
        }
        FORMULA* es = FORMULA::Const(INT(es_const));
        f = f ? FORMULA::Mul(es,f) : es;
        if (Debug_Cache_Model >= 3) {
          fprintf(TFile, "-> arl base %d, subgroup %d <-\n", ix, i);
          rg->Print(TFile);
          fprintf(TFile, "bytes = ");
          f->Print(TFile);
          fprintf(TFile, "\n");
        }
        if (rg->Rglist.Store_Count)
          rval.WFormula = rval.WFormula ? FORMULA::Add(rval.WFormula, f) : f;
        else
          rval.RFormula = rval.RFormula ? FORMULA::Add(rval.RFormula, f) : f;
      }
    }
  }

  return rval;
}

//--------------------------------------------------------------------------


static double RSolve(FORMULA*               f,
                     INT                    nloops,
                     const INT*             loops,
                     const INT64*           adj_max_iters,
                     const INT64*           fixed_iters,
                     mINT64*                t,
                     INT                    loopno);

static double Rtry(INT64                  b,
                   FORMULA*               f,
                   INT                    nloops,
                   const INT*             loops,
                   const mINT64*          adj_max_iters,
                   const mINT64*          fixed_iters,
                   mINT64*                t,
                   INT                    loopno,
                   INT                    loopno_last)
{
  double answer;
  INT i;
  for (i = loopno; i <= loopno_last; i++) {
    // this makes sense.  b may be larger, when loopno != loopno_last
    t[i] = MIN(b, adj_max_iters[loops[i]]);
  }

  if (loopno_last < nloops-1)
    answer = RSolve(f, nloops, loops, adj_max_iters, fixed_iters,
                    t, loopno_last+1);
  else {
    answer = f->Eval(nloops, t);
    Rtry_Count++;
    if (Debug_Cache_Model >= 3) {
      fprintf(TFile, "%d: Formula(", Rtry_Count);
      INT i;
      for (i = 0; i < nloops; i++) {
        fprintf(TFile, "%lld", t[i]);
	if (i < nloops - 1)
	  fprintf(TFile, ","); 
      } 
      fprintf(TFile, ") = %g\n", answer);
    }
  }

  return answer;
}

static double RSolve3(FORMULA*               f,
                      INT64                  b1,
                      INT64                  b2,
                      INT64                  b3,
                      double                 v1,
                      double                 v2,
                      double                 v3,
                      mINT64*                t1,
                      mINT64*                t2,
                      mINT64*                t3,

                      mINT64*                t,   // return value

                      INT                    nloops,
                      const INT*             loops,
                      const mINT64*          adj_max_iters,
                      const mINT64*          fixed_iters,
                      INT                    loopno,
                      INT                    loopno_last)
{
  if (v1 < v2 || v3 < v2)
    DevWarn("RSolve3 running sub-optimally -- okay");

  INT blk_step = 1;

  // case 1: range fully narrowed, so end of recursion.  We have found the
  // best b for this level (and therefore, because of the Rtry's, for all
  // levels down as well.

  if ((b1 + 1 + b1/RECT_BLOCKSIZE_UNCERTAINTY >= b2 || b1 + blk_step >= b2) &&
      (b2 + 1 + b2/RECT_BLOCKSIZE_UNCERTAINTY >= b3 || b2 + blk_step >= b3)) {
    INT i;
    for (i = loopno; i < nloops; i++)
      t[i] = t2[i];
    return v2;
  }

  INT64   bnew;
  double  vnew;
  INT64   tnew[SNL_MAX_LOOPS];
  INT i;
  for (i = 0; i < loopno; i++) {
    Is_True(t1[i] == t[i] && t2[i] == t[i] && t3[i] == t[i], ("Bug"));
    tnew[i] = t[i];
  }

  if (b2 - b1 > b3 - b2) {      // narrow range below
    bnew = (b1 + b2 + blk_step - 1)/2;
    if (blk_step > 1) {
      bnew -= bnew%blk_step;
      bnew = MAX(bnew, b1+1);
    }
    vnew = Rtry(bnew, f, nloops, loops, adj_max_iters, fixed_iters,
                tnew, loopno, loopno_last);
    if (vnew < v2)
      return RSolve3(f, b1, bnew, b2, v1, vnew, v2, t1, tnew, t2, t,
                     nloops, loops, adj_max_iters, fixed_iters,
                     loopno, loopno_last);
    else
      return RSolve3(f, bnew, b2, b3, vnew, v2, v3, tnew, t2, t3, t,
                     nloops, loops, adj_max_iters, fixed_iters,
                     loopno, loopno_last);
  }
  else {                        // narrow range above
    bnew = (b2 + b3 + blk_step - 1)/2;
    if (blk_step > 1) {
      bnew -= bnew%blk_step;
      bnew = MAX(bnew, b2+1);
    }
    vnew = Rtry(bnew, f, nloops, loops, adj_max_iters, fixed_iters,
                tnew, loopno, loopno_last);
    if (vnew < v2)
      return RSolve3(f, b2, bnew, b3, v2, vnew, v3, t2, tnew, t3, t,
                     nloops, loops, adj_max_iters, fixed_iters,
                     loopno, loopno_last);
    else
      return RSolve3(f, b1, b2, bnew, v1, v2, vnew, t1, t2, tnew, t,
                     nloops, loops, adj_max_iters, fixed_iters,
                     loopno, loopno_last);
  }
}

//--------------------------------------------------------------------------

// The goal is to minimize the funtion f.  This function is piecewise
// differentiable, and so we should take the partial derivatives and solve.
// But the function is so hairy that I don't think it's a good idea.
// Better to recursively solve this problem.  We use a heuristic, that
// the minimum must occur between the smallest value thus far seen and
// its two adjacent known values.  (This is true for a function that,
// for a given B, has a negative derivative before the min and positive
// after.)

// We also only choose bsz that are multiplies of the unroll factors.
// Actually, what we do is solve for the B's, ignoring unrolling.  When
// done, we just do bsz[i] *= unrolls[loops[i]].  (so est_iters and
// exact iters must be divided by unrolls).

// It would be nice to further choose unroll factors that had something
// sensible to do with exact iteration counts (e.g. prefer a blocksize
// of 25 when N=50).  Since that makes the search non-monotonic, that
// breaks the first paragraph and so we don't do it.

// NOTE: This whole "within 2%" thing hurts monotonicity.  In fact, the fact
// that that it's integral might be a problem?  So monotonicity cannot be
// required, only a heuristic.  We run assuming monotonicity, and if the
// assumption is ever violated, we just get a less than optimal answer.

static double RSolve(FORMULA*               f,
                     INT                    nloops,     /* e.g. 2 if B1xB2 */
                     const INT*             loops,
                     const mINT64*          adj_max_iters,
                     const mINT64*          fixed_iters,
                     mINT64*                t,             /* return value */
                     INT                    loopno)
{
  if (fixed_iters[loopno] >= 1)
    return Rtry(fixed_iters[loopno], f, nloops, loops, adj_max_iters,
                fixed_iters, t, loopno, loopno);
    
  INT   loopno_last = MAX(nloops - Max_Different_Blocksizes, loopno);

  INT64 mxiters = 1;
  INT i;
  for (i = loopno; i <= loopno_last; i++)
    mxiters = MAX(mxiters, adj_max_iters[loops[i]]);

  if (mxiters == 1)
    return Rtry(1, f, nloops, loops, adj_max_iters, fixed_iters,
                t, loopno, loopno_last);

  INT64 t1[SNL_MAX_LOOPS];
  INT64 t2[SNL_MAX_LOOPS];
  INT64 t3[SNL_MAX_LOOPS];

  for (i = 0; i < nloops; i++) {
    t3[i] = t[i];
    t2[i] = t3[i];
    t1[i] = t2[i];
  }


  INT64 nbksz = Nominal_Blocksize[nloops+1];
  INT64 start_iters = (loopno == nloops-1 && nloops > 1) ?  2*nbksz : nbksz;

  while (start_iters >= mxiters)
    start_iters /= 2;
  const INT lowest = 2;
  if (start_iters < lowest)
    start_iters = lowest;
  else
    start_iters -= start_iters%lowest;

  INT64 b1 = 0;
  INT64 b2 = start_iters;
  INT64 b3 = MIN(start_iters*2, mxiters);

  double v1 = 0.0;
  double v2 = Rtry(b2, f, nloops, loops, adj_max_iters, fixed_iters,
                   t2, loopno, loopno_last);
  double v3 = Rtry(b3, f, nloops, loops, adj_max_iters, fixed_iters,
                   t3, loopno, loopno_last);

  if (v2 <= v3) {

    // keep looking at smaller and smaller values (by half) until the
    // smallest blocksize produces a worse result.  Either we hit a
    // blocksize of 1 as the smallest, and we return it, or we have
    // the smallest being the middle blocksize, the condition required
    // for Rsolve3()

    while (b2 > lowest) {
      b1 = b2/2;
      v1 = Rtry(b1, f, nloops, loops, adj_max_iters, fixed_iters,
                t1, loopno, loopno_last);
      if (v1 > v2)
        break;

      // shift over
      b3 = b2; b2 = b1;
      v3 = v2; v2 = v1;
      for (i = loopno; i < nloops; i++) {
        t3[i] = t2[i];
        t2[i] = t1[i];
      }
    }

    if (b2 <= lowest) {
      for (i = loopno; i < nloops; i++)
        t[i] = t2[i];
      return v2;
    }
  }
  else {                // the larger was better, so increase

    while (v2 > v3) {     // so the largest b seen so far is the best.

      // Possibly we will hit the adj_max_iters before the minimum.  Here's how
      // we terminate.  If we double and hit the max, don't bother looking
      // inbetween.  Better to just use the max.

      if (b3 >= mxiters) {
        for (i = loopno; i < nloops; i++)
          t[i] = t3[i];
        return v3;
      }

      // shift everything over
      b1 = b2; b2 = b3;
      v1 = v2; v2 = v3;
      for (i = loopno; i < nloops; i++) {
        t1[i] = t2[i];
        t2[i] = t3[i];
      }

      b3 = MIN(2*b2, mxiters);
      v3 = Rtry(b3, f, nloops, loops, adj_max_iters, fixed_iters,
                t3, loopno, loopno_last);
    }
  }

  return RSolve3(f, b1, b2, b3, v1, v2, v3, t1, t2, t3, t, nloops, loops,
                 adj_max_iters, fixed_iters, loopno, loopno_last);
}

static double RSolve_Go(FORMULA*               f,
                        INT                    nloops,
                        const INT*             loops,
                        const mINT64*          adj_max_iters,
			const INT*             unrolls,
			const INT*             required_blocksize,
                        INT*                   t)
{
  mINT64  required[SNL_MAX_LOOPS];
  mINT64  tt[SNL_MAX_LOOPS];
  
  // load up required with any of the precomputed answer
  INT i; 
  for (i = 0; i < nloops; i++) {
    required[i] = required_blocksize[loops[i]];
    if (required[i] < 0 && LNO_Blocking_Size) {
      required[i] = MIN(LNO_Blocking_Size/unrolls[loops[i]],
                        adj_max_iters[loops[i]]);
    }
    else if (required[i] == 0)
      required[i] = adj_max_iters[loops[i]];
    if (required[i] >= 0)
      required[i] = MAX(required[i],2); // things work better with at least two
  }

  double answer = RSolve(f, nloops, loops, adj_max_iters, required, tt, 0);

  for (i = 0; i < nloops; i++) {
    t[i] = MIN(tt[i],INT32_MAX);
  }

  return answer;
}

//----------------------------------------------------------------------------
// Compute_Do_Overhead:
//
// This computes the cycles per iteration for the given loop structure.
// Note that the assumption is that the loop setup time is Loop_Overhead
// cycles.  Thus, unrolling the inner loop does not reduce overhead.
// Since we don't do inner loop unrolling, this model is in fact correct,
// but it's slightly inaccurate further out.
//
// Note that it is not feasible to say "overhead due to L1" or "overhead
// due to L2".  There is interaction between them, not a simple sum as in
// computing cache miss penalties.  So this routine just computes the total
// loop overhead and the caller to this routine must decide how to use this
// result
//
// The formula is this:
//         SUM   ( loop_overhead / (iterations from all loops inside )
//        loops
//
// Note that since the strip sizes are in terms of original iteration counts,
// we have to be careful, especially in the face of unrolling.
//
// For example, no blocking:
//
//         do i = 1, N1, 2
//          do j = 1, N2, 2
//           do k = 1, N3
//
// If o is the loop overhead, then, intuitively, we have overhead cycles/iter:
//
//         o/[(4*N3)]                      k loop
//           + o/[(4*N3)*(N2/2)]           j loop
//           + o/[(4*N3)*(N2/2)*(N1/2)]    i loop
//
// So you see that the unrolling factors interact in a complex way with the
// loop bounds.
//
// Now, suppose we have unrolls and tiling
//
//     do k'' = 1, N3
//      do i'' = 1, N1
//       do j' = 1, N2
//        do k' = k'', k''+B4-1
//         do i = i'', i''+B3-2, 2      (unrolled 2)
//          do j = j', j'+B2-2, 2       (unrolled 2)
//           do k = k', k'+B1-1
//
// If o is the loop overhead, then, intuitively, we have overhead cycles/iter:
// (e.g. note for the k' loop, it's the same denominator as for the i loop,
// but divided by B4/B1, which is how many times the k loop is executed)
//
//         o/[(4*B1)]                             k loop
//           + o/[(4*B1)*(B2/2)]                  j loop
//           + o/[(4*B1)*(B2/2)*(B3/2)]           i loop
//           + o/[(4*B4)*(B2/2)*(B3/2)]           k' loop
//           + o/[(4*B4)*(N2/2)*(B3/2)]           j' loop
//           + o/[(4*B4)*(N2/2)*(N1/2)]           i'' loop
//           + o/[(4*N3)*(N2/2)*(N1/2)]           k'' loop
//
//----------------------------------------------------------------------------

static double Compute_Do_Overhead(INT          depth,
                                  INT          stripdepth,
                                  INT          nstrips,
                                  const INT*   iloop,
                                  const INT*   stripsz,
                                  const INT*   unrolls,
                                  const INT64* est_iters,
                                  const INT*   permute_order)
{
  double ovhd = 0.0;

  if (nstrips == 0)
    stripdepth = depth+1;

  // the easy part: no worries about blocking.  Whether *this* loop is
  // unrolled plays no part in loop overhead, because it's Loop_Overhead
  // cycles for the *setup* of the loop, not each go-around.

  INT   unrolls_so_far = 1;
  INT i;
  for (i = 0; i < stripdepth; i++) {
    double iters = unrolls_so_far;
    INT j;
    for (j = i; j <= depth; j++)
      iters *= est_iters[permute_order[j]];
    if (iters > LNO_Small_Trip_Count)
      ovhd += Loop_Overhead/iters;
    unrolls_so_far *= unrolls[permute_order[i]];
  }

  // Now, handle each strip.  None of these loops are unrolled, so we can
  // use unrolls_so_far.  The iterations this loop has is the stripsize of
  // this loop (further strips inside are irrelevant).  Likewise for each
  // loop inside stripdepth: the largest stripsize inside, or, if no such
  // thing, the est iters.  That's because all strips are implicity at
  // stripdepth.
  INT s;
  for (s = 0; s < nstrips; s++) {
    double iters = unrolls_so_far;
    for (INT i = stripdepth; i <= depth; i++) {
      INT ss;
      for (ss = s-1; ss >= 0; ss--) {
        if (iloop[ss] == permute_order[i])
          break;
      }
      iters *= (ss >= 0) ? stripsz[ss] : est_iters[permute_order[i]];
    }
    if (iters > LNO_Small_Trip_Count)
      ovhd += Loop_Overhead/iters;
  }

  // Finish off with the final stripped loops.  Here again, if the loop us
  // unrolled here or above, it doesn't help.  Also, the iteration counts
  // are the minimum strip size or est_iters.

  for (i = stripdepth; i <= depth; i++) {
    double iters = unrolls_so_far;
    INT j;
    for (j = i; j <= depth; j++) {
      INT ss;
      for (ss = nstrips-1; ss >= 0; ss--) {
        if (iloop[ss] == permute_order[j])
          break;
      }
      iters *= (ss >= 0) ? stripsz[ss] : est_iters[permute_order[i]];
    }
    if (iters > LNO_Small_Trip_Count)
      ovhd += Loop_Overhead/iters;
    unrolls_so_far *= unrolls[permute_order[i]];
  }

  return ovhd;
}

BOOL Do_Copying(const ARRAY_REF*        /*arl*/,
                INT                     /*mhd_level*/,
                INT                     /*nloops*/,
                const INT*              /*loops*/
		)
{
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Has_Outer_Reuse_In_SNL 
// FUNCTION: Returns TRUE if some loop in the SNL outside the 'nloops' loops 
//   in 'loops' had reuse.  The array 'originally_has_reuse' of length 
//   'depth + 1' records which loops were found to have reuse.  Only the 
//   innermost (nloops_in_snl, ie ninners + ntiles) loops (i.e those
//   constituting the SNL) are checked. 
//-----------------------------------------------------------------------

static BOOL Has_Outer_Reuse_In_SNL(const INT*  loops, 
                                   INT         nloops,
                                   INT         depth,
                                   INT         nloops_in_snl,
                                   const INT*  available_order,
                                   INT         available_depth,
                                   const BOOL* originally_has_reuse)
{
  if (depth + 1 == nloops)   // quick optimization
    return FALSE;

  BOOL outer_reuse[MAX_DEPTH];

  // loops with reuse marked with 1
  INT i;
  for (i = 0;  i <= depth; i++)
    outer_reuse[i] = originally_has_reuse[i];
  // loops outside the nest back to 0
  for (i = 0; i <= available_depth - nloops_in_snl; i++)
    outer_reuse[available_order[i]] = 0;
  // loops in the block back to 0
  for (i = 0; i < nloops; i++) 
    outer_reuse[loops[i]] = 0;
  // any remaining 1 must be reuse outside the block but inside the SNL
  for (i = 0; i <= depth; i++)
    if (outer_reuse[i]) 
      return TRUE; 
  return FALSE;  
}

//-----------------------------------------------------------------------
// NAME: Fits_In_The_Cache
// FUNCTION: Returns TRUE if the subnest 'loops' of 'nloops' loops fits 
//   in the cache, FALSE otherwise.  In either case, the number of bytes
//   estimated is returned in '*bytes'.   
// Other parameters are inherited from the call to 'Nest_Model'.  
//-----------------------------------------------------------------------
static BOOL Fits_In_The_Cache(const ARRAY_REF* arl,
			      const INT*       loops,
			      const INT        nloops, 
			      const mINT64*    est_iters,
			      const mINT64*    max_iters,
			      const INT*       arl_stripsz,
                              const INT*       unrolls,
			      const INT        depth, 
                              const INT        stripdepth,
                              const INT*       permute_order,
                              BOOL             use_tlb,
			      double*          bytes)
{

  if (Debug_Cache_Model) {
    fprintf(TFile, "+++++ FITS IN THE "); 
    if (use_tlb) 
      fprintf(TFile, "TLB ");
    else 
      fprintf(TFile, "CACHE "); 
    fprintf(TFile, "{"); 
    INT i;
    for (i = 0; i < nloops - 1; i++) 
      fprintf(TFile, "%d,", loops[i]);
    fprintf(TFile, "%d}:", loops[nloops - 1]); 
    fprintf(TFile, "+++++\n"); 
  } 

  if (use_tlb && !Cur_Mhd->TLB_Valid()) {
    *bytes = 0;
    return TRUE;
  }
  INT i;
  for (i = 0; i < nloops; i++) 
    if (max_iters[loops[i]] == UNBOUNDED_ITERS)
      return FALSE; 
  int* iters = (int*) alloca(sizeof(int)*nloops);
  for (i = 0; i < nloops; i++) {
    // est_iters and arl_stripsz both divided through by unrolls, so this is
    // fine as is, so est_iters/arl_stripsz is correct.  We wish to see if
    // all iters in loops fit in the cache, so est_iters of 1 and 0 both mean
    // to use iters==est_iters, otherwise divide by arl_stripsz.
    INT ii = loops[i];
    iters[i] = est_iters[ii];
    if (arl_stripsz[ii] > 1) {
      iters[i] = Divceil(iters[i], arl_stripsz[ii]);
      iters[i] = MAX(iters[i], 2); // est_iters < stripsz -- not impossible
    }
  }
  COMPUTE_FOOTPRINT_RVAL F1; 
  F1 = Compute_Footprint(arl, nloops, loops, arl_stripsz, est_iters, max_iters,
    unrolls, depth, stripdepth, permute_order, -1, iters[0], use_tlb, -1); 
  double cache_bytes = F1.AllFormula() == NULL ? 0.0 :
                       F1.AllFormula()->Eval(nloops - 1, &iters[1]);
  *bytes = cache_bytes; 

  if (Debug_Cache_Model) {
    fprintf(TFile, "+++++ END FITS IN THE "); 
    if (use_tlb) 
      fprintf(TFile, "TLB ");
    else 
      fprintf(TFile, "CACHE "); 
    fprintf(TFile, "{"); 
    INT i;
    for (i = 0; i < nloops - 1; i++) 
      fprintf(TFile, "%d,", loops[i]);
    fprintf(TFile, "%d}:", loops[nloops - 1]); 
    fprintf(TFile, "+++++\n"); 
  } 
#ifndef _FIX_CACHE_MODEL_
  return (cache_bytes <= Cur_Mhd->Effective_Size) ? TRUE : FALSE; 
#else  
  // TODO (nenad, 03/22/99):
  // This is so obviusly wrong when modeling TLB. We will use ~5K as 
  // its size, instead of ~2M. The sad part is that the fix below 
  // actually may result in worse performance. One likely reason for
  // that is that the model keeps adding loops to the nest until they
  // don't fit in the cache, but the entire cost seems to be attributed
  // to the original nest. That also could be fixed, but it triggers
  // additional problems in the model, such as negative cache estimates.
  double eff_size = use_tlb ? 
                    Cur_Mhd->TLB_Entries * Cur_Mhd->Page_Size :
                    Cur_Mhd->Effective_Size;
  return (cache_bytes <= eff_size) ? TRUE : FALSE; 
#endif // _FIX_CACHE_MODEL_

}

// Compute_Miss_Bytes
//
//    Return a formula for the bytes/iter to be attributed to
//    misses.   We pass in three register parameters: the reg into which
//    to compute the read miss bytes/iter; the reg into which to compute
//    the write bytes per iter, and the first free reg.  Compute miss bytes
//    may use any register from this number and above without fear of
//    overwriting something important.
//
//    Return NULL if the answer is a constant, in which case the constant
//    is returned in rbytes_per_iter, wbytes_per_iter.  Otherwise, those
//    values are not used.
//
// Here's more detail about the computation than occurs in the comments
// at the top of the file.  First pretend we have two loops.
//
//    F1 = footprint for all iterations of both loops
//    F2 = footprint for all iterations of inner loop only (one iter of outer)
//    v = iterations of outer loop (be careful when v==1, which is possible)
//    newdata = (F1-F2)/(v1-1) = new data brought in each iter of outer loop
//    d = how many iterations of outer loop touch the same data (excl temporal)
//    requirements = F2 + d*newdata = how big the cache must be
//
// Roughly speaking, the data we need per iteration of the inner loop is F1/v1.
// But we lose some reuse because of cache capacity misses.  In the outermost
// loop, use use the formula that when we are below the effective cache size,
// we lose
//          frac_reuse_lost1 = requirements * penalty1 / size
// fraction of reuse.  We add in an additional like formula when we exceed
// the effective size.
//          frac_reuse_lost2 = (requirements-effective_size) * penalty2 / size
// So
//     reused = (F2 - newdata)
//     reused_lost = (frac_reuse_lost1 + frac_reuse_lost2) * reused
//
// This has been a repeat of the top of the file, more or less.  What
// happens if there are three more more loops.  The rough number of cycles
// is the same, but the reuse lost has to be summed over loops further and
// further in, and multiplied by the appropriate number of iterations.

static FORMULA* Compute_Miss_Bytes(const ARRAY_REF*        arl,
                                   INT                     nloops,
                                   const INT*              loops,
                                   const mINT64*           est_iters,
                                   const mINT64*           max_iters,
                                   const INT*              arl_stripsz,
                                   const INT*              unrolls,
                                   INT                     depth,
                                   INT                     stripdepth,
                                   const INT*              permute_order,
                                   INT                     mhd_level,
                                   INT64                   v1,
                                   BOOL                    using_tlb,
                                   INT                     read_sreg,
                                   INT                     write_sreg,
                                   INT                     first_free_sreg,
                                   double*                 rbytes_per_iter,
                                   double*                 wbytes_per_iter)
{
  double  size;
  double  eff_size;
  INT     line_size;
  double  basic_middle_penalty;
  double  additional_middle_penalty;

  *rbytes_per_iter = 0.0;
  *wbytes_per_iter = 0.0;

  if (using_tlb) {
    size = Cur_Mhd->TLB_Entries * Cur_Mhd->Page_Size;
    eff_size = Cur_Mhd->TLB_Entries * Cur_Mhd->Page_Size;
    line_size = Cur_Mhd->Page_Size;
    basic_middle_penalty = BASIC_MIDDLE_PENALTY_TLB;
    additional_middle_penalty = ADDITIONAL_MIDDLE_PENALTY_TLB;
  }
  else {
    BOOL do_copying = Do_Copying(arl, mhd_level, nloops, loops);
    size = Cur_Mhd->Size;
    eff_size = do_copying ? 0.9 * Cur_Mhd->Size : Cur_Mhd->Effective_Size;
    line_size = Cur_Mhd->Line_Size;
    if (Cur_Mhd->Type == MHD_TYPE_MEM) {
      basic_middle_penalty = BASIC_MIDDLE_PENALTY_MEM;
      additional_middle_penalty = ADDITIONAL_MIDDLE_PENALTY_MEM;
    }
    else {
      basic_middle_penalty = BASIC_MIDDLE_PENALTY;
      additional_middle_penalty = ADDITIONAL_MIDDLE_PENALTY;
    }
  }

  if (nloops == 1) {
    COMPUTE_FOOTPRINT_RVAL F1;

    F1 = Compute_Footprint(arl, nloops, loops, arl_stripsz,
                           est_iters, max_iters,
                           unrolls, depth, stripdepth, permute_order, -1,
			   v1, using_tlb, nloops > 1 ? loops[0] : -1);
    if (F1.RFormula != NULL)
      *rbytes_per_iter = F1.RFormula->Eval(0, (const double*)NULL) / v1;
    if (F1.WFormula != NULL)
      *wbytes_per_iter = F1.WFormula->Eval(0, (const double*)NULL) / v1;
    if (Debug_Cache_Model >= 2) {
      fprintf(TFile, "Compute_Miss_Bytes returns one deep with r=%g,w=%g\n",
              *rbytes_per_iter, *wbytes_per_iter);
    }
    return NULL;
  }

  // nloops > 1 from here down.

  // Begin by computing the footprints from here in.

  COMPUTE_FOOTPRINT_RVAL* F =
    CXX_NEW_ARRAY(COMPUTE_FOOTPRINT_RVAL, nloops, &LNO_local_pool);
  INT lp; 
  for (lp = 0; lp < nloops; lp++) {
    F[lp] = Compute_Footprint(arl, nloops-lp, loops+lp, arl_stripsz,
                              est_iters, max_iters,
                              unrolls, depth, stripdepth, permute_order, lp-1,
                              v1, using_tlb, lp == 0 ? loops[0] : -1);

    if (Debug_Cache_Model >= 2) {
      fprintf(TFile, "<tlb=%d> F[%d] = ", using_tlb, lp);
      F[lp].Print(TFile);
    }

    if (lp == 0 && F[0].AllFormula() == NULL) {
      if (Debug_Cache_Model >= 2)
        fprintf(TFile, "Compute_Miss_Bytes F1 found no data -- return 0s\n");
      CXX_DELETE_ARRAY(F, &LNO_local_pool);
      return NULL;
    }
  }

  // First we compute all the footprints into the register set.
  // nloops < LNO_MAX_DO_LOOP_DEPTH, so we'll use that fact.

  INT fr_sreg0 = first_free_sreg;
  INT fw_sreg0 = first_free_sreg + LNO_MAX_DO_LOOP_DEPTH;
  INT frw_sreg0 = first_free_sreg + 2*LNO_MAX_DO_LOOP_DEPTH;
  INT newdata_sreg0 = first_free_sreg + 3*LNO_MAX_DO_LOOP_DEPTH;
  INT requirements_sreg0 = first_free_sreg + 4*LNO_MAX_DO_LOOP_DEPTH;

  FORMULA* initialization = NULL;

  for (lp = 0; lp < nloops; lp++) {
    FORMULA* newf = FORMULA::Comma3(FORMULA::Set0(fr_sreg0+lp, F[lp].RFormula),
                                    FORMULA::Set0(fw_sreg0+lp, F[lp].WFormula),
                                    FORMULA::Set0(frw_sreg0+lp,
                                     FORMULA::Add(FORMULA::Use(fr_sreg0+lp),
                                                  FORMULA::Use(fw_sreg0+lp))));
    if (initialization == NULL)
      initialization = newf;
    else
      initialization = FORMULA::Comma(initialization, newf);
  }

  // Now compute the newdata and requirements

  for (lp = 0; lp < nloops-1; lp++) {
    if (lp == 0 && v1 <= 1)
      continue;       // no reuse lost here

    // compute new data.  This requires the number of iterations in this
    // loop.  newdata = (F1 - F2) / (v1 - 1)

    FORMULA* nk_m_1 = (lp == 0) ? FORMULA::Const(v1-1) :
                                  FORMULA::Sub(FORMULA::Var(lp-1),
                                               FORMULA::Const(1));

    FORMULA* fnewdata = FORMULA::Div(FORMULA::Sub(FORMULA::Use(frw_sreg0+lp),
                                                 FORMULA::Use(frw_sreg0+lp+1)),
                                     nk_m_1);

    // if either of these conditions hold, there's no reuse (and we don't
    // want to divide by zero).

    FORMULA* cond = FORMULA::Lt(FORMULA::Use(frw_sreg0+lp), FORMULA::Const(1));
    if (lp > 0)
      cond = FORMULA::Or(cond,
                         FORMULA::Le(FORMULA::Var(lp-1), FORMULA::Const(1)));
    fnewdata = FORMULA::Cond(cond, FORMULA::Const(0), fnewdata);

    // compute the requirements:  F2 + newdata * (D-1)
    
    FORMULA* requirements = FORMULA::Use(frw_sreg0+lp+1);
    if (F[lp].D > 2)
      requirements = FORMULA::Add(requirements,
                                  FORMULA::Mul(FORMULA::Use(newdata_sreg0+lp),
                                               FORMULA::Const(F[lp].D-1)));
    else if (F[lp].D == 2)
      requirements = FORMULA::Add(requirements,FORMULA::Use(newdata_sreg0+lp));

    initialization = FORMULA::Comma3(initialization,
                                     FORMULA::Set(newdata_sreg0+lp, fnewdata),
                                     FORMULA::Set(requirements_sreg0+lp,
                                                  requirements));
  }

  // Now we know the footprints.  We start with the simplest effect.  The
  // footprint required to execute all iterations of the outermost loop lp==0
  // is the footprint F[0].  Per iteration of lp==0, it's thus F[0]/v1.
  // So the only question is how to partition that into fraction of reads
  // and fraction of writes.  One guess might be the ratio of reads to writes
  // in that footprint.  That's not really reasonable, though.  If lp==0
  // has lots of writes that are used once, that counts more than writes that
  // occur over and over (and occasionally miss).  We really want to use the
  // footprint F[1].  But that could be null, and if there really are writes
  // in the outer loop, they should count a little.  So we partition read
  // misses and write misses in the ratio
  //          F[0].reads + v0*F[1].reads + v0*v1*F[2].reads ... ::
  //          F[0].writes + v0*F[1].writes + v0*v1*F[2].writes ...
  // which means that what's going on in the innermost loop is most crucial
  // to determining how many writes there are.

  FORMULA* reuse_lost_rbytes;
  FORMULA* reuse_lost_wbytes;
  {
    FORMULA* frac_rnum = FORMULA::Use(fr_sreg0);
    FORMULA* frac_wnum = FORMULA::Use(fw_sreg0);
    FORMULA* frac_denom = FORMULA::Use(frw_sreg0);
    FORMULA* factor = FORMULA::Var(0);
    INT lp;
    for (lp = 1; lp < nloops; lp++) {
      frac_rnum = FORMULA::Add(frac_rnum,
                               FORMULA::Mul(factor->Duplicate(),
			                    FORMULA::Use(fr_sreg0+lp)));
      frac_wnum = FORMULA::Add(frac_wnum,
                               FORMULA::Mul(factor->Duplicate(),
			                    FORMULA::Use(fw_sreg0+lp)));
      frac_denom = FORMULA::Add(frac_denom,
                               FORMULA::Mul(factor->Duplicate(),
			                    FORMULA::Use(frw_sreg0+lp)));
      factor = FORMULA::Mul(factor, FORMULA::Var(lp));
    }
    FORMULA* frac_reads = FORMULA::Div(frac_rnum,
                                       frac_denom->Duplicate());
    FORMULA* frac_writes = FORMULA::Div(frac_wnum, frac_denom);
    reuse_lost_rbytes = FORMULA::Mul(FORMULA::Div(FORMULA::Use(frw_sreg0),
                                                  FORMULA::Const(v1)),
                                     frac_reads);
    reuse_lost_wbytes = FORMULA::Mul(FORMULA::Div(FORMULA::Use(frw_sreg0),
                                                  FORMULA::Const(v1)),
                                     frac_writes);
  }

  // ... we have to compute the reuse lost
  // for loops 0 though nloops-2.  The inner loop doesn't lose any reuse,
  // so long as the body itself doesn't overflow the cache or have cache
  // interference.  It could, but I'll assume it doesn't.
  // We lose reuse because of exceeding cache capacity to large extent and,
  // to a much smaller extent, just random inverference when using less.
  // See comments up top.

  for (lp = 0; lp < nloops-1; lp++) {
    if (lp == 0 && v1 <= 1)
      continue;       // no reuse lost here

    // Reuse per iteration of lp.  reuse/iteration is F2 - newdata.
    // Since there are approximations in our model, this idea that
    // newdata might equal F2 is questionable.  We cover our butts slightly.

    FORMULA* reuse = FORMULA::Max(FORMULA::Sub(FORMULA::Use(frw_sreg0+lp+1),
                                               FORMULA::Use(newdata_sreg0+lp)),
                                  FORMULA::Const(line_size));

    // we actually need reuse per iteration of lp == 0.  So multiply through.
    INT llp;
    for (llp = 1; llp <= lp; llp++)
      reuse = FORMULA::Mul(reuse, FORMULA::Var(llp-1));

    // Compute the reuse lost.

    double   c1 = basic_middle_penalty / size;
    double   c2 = additional_middle_penalty / size;

    FORMULA* frac_reuse_lost1 = NULL;
    FORMULA* frac_reuse_lost2 = NULL;

    if (c1)
      frac_reuse_lost1 = FORMULA::Mul(FORMULA::Use(requirements_sreg0+lp),
                                      FORMULA::Const(c1));
    else
      frac_reuse_lost1 = FORMULA::Const(0);

    if (c2)
      frac_reuse_lost2 =
        FORMULA::Max(FORMULA::Const(0),
                    FORMULA::Mul(FORMULA::Sub(FORMULA::Use(requirements_sreg0+lp),
                                              FORMULA::Const(eff_size)),
                                 FORMULA::Const(c2)));
    else
      frac_reuse_lost2 = FORMULA::Const(0);

    FORMULA* reuse_lost = FORMULA::Mul(FORMULA::Add(frac_reuse_lost1,
                                                    frac_reuse_lost2), reuse);

    // Now we compute the cycles lost to reuse.  We need first the cycles
    // per byte.  That's the weighted average of the read and write miss cost.

    FORMULA* frac_reads = FORMULA::Div(FORMULA::Use(fr_sreg0+lp+1),
                                       FORMULA::Use(frw_sreg0+lp+1));
    FORMULA* frac_writes = FORMULA::Div(FORMULA::Use(fw_sreg0+lp+1),
                                        FORMULA::Use(frw_sreg0+lp+1));

    // finally, we get the cache interference cycles per iteration of lp==0
    // due to lp.

    FORMULA* reuse_lost_rbytes_lp = FORMULA::Mul(reuse_lost->Duplicate(),
                                                 frac_reads);
    FORMULA* reuse_lost_wbytes_lp = FORMULA::Mul(reuse_lost,
                                                 frac_writes);

    // there are various ways to create a useless formula.  For example, if
    // v_{lp-1} <= 1, there's no reuse.  (We checked for that already with
    // the lp==0 case.)  Also if the footprint frw_sreg+lp is less than a byte.

    FORMULA* cond = FORMULA::Lt(FORMULA::Use(frw_sreg0+lp), FORMULA::Const(1));
    if (lp > 0)
      cond = FORMULA::Or(cond,
                         FORMULA::Le(FORMULA::Var(lp-1), FORMULA::Const(1)));
    reuse_lost_rbytes_lp = FORMULA::Cond(cond->Duplicate(), FORMULA::Const(0),
                                         reuse_lost_rbytes_lp);
    reuse_lost_wbytes_lp = FORMULA::Cond(cond, FORMULA::Const(0),
                                         reuse_lost_wbytes_lp);

    reuse_lost_rbytes = FORMULA::Add(reuse_lost_rbytes,reuse_lost_rbytes_lp);

    reuse_lost_wbytes = FORMULA::Add(reuse_lost_wbytes,reuse_lost_wbytes_lp);
  }

  // We actually want an answer per iteration of the inner loop, not of lp0.
  // So we have to divide through by v0 * ... * v(nloops-2)

  FORMULA* variters = FORMULA::Var(0);
  INT ii;
  for (ii = 1; ii < nloops - 1; ii++)
    variters = FORMULA::Mul(variters, FORMULA::Var(ii));

  reuse_lost_rbytes = FORMULA::Set(read_sreg,
                      FORMULA::Div(reuse_lost_rbytes, variters->Duplicate()));
  reuse_lost_wbytes = FORMULA::Set(write_sreg,
                      FORMULA::Div(reuse_lost_wbytes, variters));

  return FORMULA::Comma3(initialization, reuse_lost_rbytes, reuse_lost_wbytes);
}

static INT64 Estimate_Middle_Iters(INT           loop,
                                   INT           nloops,
                                   const mINT64* est_iters,
                                   const mINT64* max_iters,
                                   const INT*    arl_stripsz)
{
  // roughly est_iters/arl_stripsz.  Of course, if est_iters=0, that's
  // as good as one for the middle loop.

  INT64 v1 = est_iters[loop];
  if (arl_stripsz[loop] > 1) {
    v1 = Divceil(INT(v1), arl_stripsz[loop]);
    v1 = MAX(v1, 2);	// est_iters < stripsz -- not impossible
  }
  if (max_iters[loop] == UNBOUNDED_ITERS) {

    // we have no idea how many iterations.  The loops further in may
    // have fewer iterations.  That's ok -- it's called blocking.  But
    // if they have more iterations below (big blocks) and if we really
    // have no idea how many iterations we are dealing with here, then
    // we'd better not underestimate, or we artifically punish this outer
    // loop, seriously skewing our answers.  Ths_reuseen again, the blocksize
    // may end up being huge, and the nominal blocksize computation
    // may be outragous.  If nloops is 1, then try to minimize overheads.

    // TODO OK: note that if we have do i = 1,200; do j = 1,n, then the
    // guessed value of n may differ from 200.  That will tend to favor
    // or disfavor loop j arbitrarily.  Doesn't matter much, but edge
    // effects do count.

    // The nominal blocksize is a bit of an underestimate.  Also, it's
    // better to overestimate.  So use a multiple of the nominal blocksize.
    // Also, again we don't know the number of iterations, so it may be
    // a lot.  Don't use a remarkably small number for no reason.

    v1 = MAX(v1, 2*Nominal_Blocksize[nloops]);
    v1 = MAX(v1, 50);
  }
  return v1;
}

// Compute_Actual_Miss_Bytes:  If it doesn't fit in the cache, just
// call Compute_Miss_Bytes to get the right answer.  Otherwise,
// keep adding loops further out until it finally doesn't fit in the cache.
//
// This routine returns the formula, or NULL if the formula is a constant,
// in which case it returns it in cycles_per_iter_const

static FORMULA*
Compute_Actual_Miss_Bytes(const ARRAY_REF*        arl,
                          INT                     nloops,
                          const INT*              loops,
                          const mINT64*           est_iters,
                          const mINT64*           max_iters,
                          const INT*              unrolls,
                          const INT*              arl_stripsz,
                          INT                     depth,
                          INT                     ninners,
                          INT                     ntiles,
                          INT                     stripdepth,
                          const INT*              permute_order,
                          const INT*              available_order,
                          INT                     available_depth,
                          INT                     mhd_level,
                          INT64                   v1,
                          BOOL                    using_tlb,
                          const BOOL*             originally_has_reuse,
                          INT                     read_sreg,
                          INT                     write_sreg,
                          INT                     first_free_sreg,
                          double*                 rbytes_per_iter_const,
                          double*                 wbytes_per_iter_const)
{
  if (Debug_Cache_Model >= 2) {
    fprintf(TFile, "\n+++++ COMPUTING ACTUAL MISS BYTES FOR "); 
    if (using_tlb)
      fprintf(TFile, "TLB ");
    else 
      fprintf(TFile, "CACHE "); 
    fprintf(TFile, "{");
    INT i;
    for (i = 0; i < nloops; i++) {
      fprintf(TFile, "%d", loops[i]);
      if (i < nloops - 1)
	fprintf(TFile, ","); 
    } 
    fprintf(TFile, "}\n");
  }

  double   bytes = 0.0;

  FmtAssert(nloops <= available_depth+1,
            ("Bad nloops=%d avaliable_depth=%d", nloops, available_depth));

  // The normal case.  If there are no further loops outside this
  // block, or the loops outside have no reuse, or the data in this
  // block (without blocking) doesn't fit in the cache, then we only
  // need look inside this loop to compute the miss penalty.  Why do we
  // do this?  Because if we had 'do i=1,N; do j=1,10 a(j)' and we were
  // examining the cost of j innermost, we don't want to think the cost
  // is that of a stride one loop, when really because j is only from 1
  // to 10, the cost is almost free because of reuse in i.

  if (nloops == ntiles+ninners ||
      !Has_Outer_Reuse_In_SNL(loops, nloops, depth, ninners+ntiles,
        available_order, available_depth, originally_has_reuse) ||
      !Fits_In_The_Cache(arl, loops, nloops, est_iters,
                         max_iters, arl_stripsz, unrolls, depth, stripdepth,
                         permute_order, using_tlb, &bytes)
#ifdef _FIX_CACHE_MODEL_
      || using_tlb
#endif
     ) {

    // TODO (nenad, 03/22/99):
    // Adding loops to the nest just because it fits in the cache
    // seems to be bogus. If loop bounds are completely unknown
    // we can choose estimates that will spill out of cache. But,
    // when we know the loop bounds (or at least their upper limits),
    // it's silly not to accept that fact that something will fit in
    // the cache. In particular, this will often happen with TLB.
    // 
    // Skipping this branch when 'using_tlb' and falling through to
    // the normal case, resulted in strange behavior, such as negative
    // cache estimates. It needs to be investigated if this was due to
    // genuine bugs, or the model just wasn't meant to deal with zero
    // costs. It could be worth trying to assign at least cold-start
    // cost to the nests that fit in the cache/TLB.

    if (Debug_Cache_Model) 
      fprintf(TFile, 
	"HAS NO OUTER REUSE OR DOES NOT FIT IN CACHE. NORMAL CASE.\n\n"); 
    if (Debug_Cache_Model)
      fprintf(TFile, "+++++ COMPUTING MISS BYTES.  NORMAL CASE. +++++\n");  
    FORMULA* fp = Compute_Miss_Bytes(arl, nloops, loops, est_iters, max_iters,
      arl_stripsz, unrolls, depth, stripdepth, permute_order,
      mhd_level, v1, using_tlb, read_sreg, write_sreg, first_free_sreg,
      rbytes_per_iter_const, wbytes_per_iter_const);
    if (Debug_Cache_Model)
      fprintf(TFile, "+++++ END COMPUTING MISS BYTES.  NORMAL CASE. +++++\n\n");  
    return fp; 
  }

  // We need to include the next loop further out (and if necessary,
  // the next, and the next ...) until the conditions are such that
  // our model will be accurate.  

  INT* future_order = (INT*) alloca(sizeof(INT)*(available_depth+1));
  INT  ao = 0;
  INT i;
  for (i = 0; i < nloops; i++)
    future_order[available_depth+1-nloops+i] = loops[i];
  for (i = 0; i <= available_depth; i++) {
    INT j;
    for (j = 0; j < nloops; j++) {
      if (loops[j] == available_order[i])
        break;
    }
    if (j >= nloops)
      future_order[ao++] = available_order[i];
  }
  FmtAssert(ao + nloops == available_depth + 1, ("Bug"));
  INT lcnt;
  for (lcnt = nloops + 1; lcnt <= ninners+ntiles; lcnt++) {
    if (Debug_Cache_Model >= 2)
      fprintf(TFile, "ATTEMPTING TO ADD LOOP {%d} TO SNL\n", 
	future_order[available_depth + 1 - lcnt]); 

    if (!Has_Outer_Reuse_In_SNL(&future_order[available_depth+1-lcnt], lcnt,
          depth, ninners+ntiles, available_order, available_depth,
          originally_has_reuse)) {
      if (Debug_Cache_Model) 
        fprintf(TFile, "NO OUTER USE ON SNL. SEARCH COMPLETE\n\n"); 
      break;
    } 
    if (!Fits_In_The_Cache(arl, &future_order[available_depth+1-lcnt], lcnt,
          est_iters, max_iters, arl_stripsz, unrolls, depth, stripdepth,
          permute_order, using_tlb, &bytes)) {
      if (Debug_Cache_Model) 
        fprintf(TFile, "DOES NOT FIT IN CACHE. SEARCH COMPLETE\n\n"); 
      break;
    } 
  }
  if (lcnt == ninners + ntiles + 1)
    lcnt = nloops; 

  INT64 v1new = Estimate_Middle_Iters(future_order[available_depth+1-lcnt],
                                      lcnt, est_iters, max_iters, arl_stripsz);
  if (Debug_Cache_Model)
    fprintf(TFile, "+++++ COMPUTING MISS BYTES. EXTENDED CASE. +++++\n");  
  FORMULA* rval = 
    Compute_Miss_Bytes(arl, lcnt, &future_order[available_depth+1-lcnt],
                       est_iters, max_iters, arl_stripsz, unrolls,
                       depth, stripdepth, permute_order,
                       mhd_level, v1new, using_tlb,
                       read_sreg, write_sreg, first_free_sreg,
                       rbytes_per_iter_const, wbytes_per_iter_const);

  if (Debug_Cache_Model)
    fprintf(TFile, "+++++ END COMPUTING MISS BYTES.  EXTENDED CASE. +++++\n\n");
  if (rval != NULL) {
    // we know it's adequate to evaluate assuming no blocking when we've
    // added loops.
    double* iters = (double*) alloca(sizeof(double)*lcnt);
    INT i;
    for (i = 1; i < lcnt; i++) {
      INT    loop = future_order[available_depth+1-lcnt+i];
      if (unrolls[loop] <= 1)
        iters[i-1] = est_iters[loop];
      else
        iters[i-1] = (est_iters[loop] + unrolls[loop] - 1)/unrolls[loop];
    }
    rval->Eval(lcnt-1, iters);           // set read_sreg and write_sreg
    *rbytes_per_iter_const =
      FORMULA::Use(read_sreg)->Eval(0, (const double *)NULL);
    *wbytes_per_iter_const =
      FORMULA::Use(write_sreg)->Eval(0, (const double *)NULL);
    if (Debug_Cache_Model >= 2) {
      fprintf(TFile, "+++++ EXTENDED ACTUAL MISS BYTES RESULTS\n");
      fprintf(TFile, "  Read Bytes Per Iteration %g\n", 
	*rbytes_per_iter_const); 
      fprintf(TFile, "  Write Bytes Per Iteration %g\n", 
        *wbytes_per_iter_const); 
      fprintf(TFile, "+++++ END EXTENDED ACTUAL MISS BYTES RESULTS\n\n");
    } 
  }

  return NULL;
}

// Nest model: compute best blocksizes and costs associated with that,
// given that we want to strip loops[1] ... loops[nloops-1] and put them
// outside loop[0].
//
//    arl:             the usual
//    nloops:          number of loops in the tile (one more than # strips)
//    loops[i]:        loop[i] is the depth of the corresponding loop
//    est_iters[d]:    number of iterations we think loop at depth d has.
//    max_iters[d]:    the largest block that makes sense.
//    arl_stripsz[d]:  if blocking further in was done for the loop with
//                     depth d, then the arl does not include that so we
//                     need to compensate.  E.g. if loop d has its biggest
//                     strip 30, but was unrolled three, then the arl is
//                     missing 10 replications.  If there was no strip,
//                     the it's either 1 or 0.  We use 1 to indicate
//                     actually 1: the loop is not in the inner tile.
//                     The value 0 says that it's in the inner tile, but
//                     not stripped.  (Presumable it goes est_iters[].)
//                     So arl_stripsz[] != 1 means involved in a tile
//                     somehow, but arl_stripsz[] > 1 means stripped.
//    depth:           arl_stripsz[0..depth]
//    stripdepth:      the usual
//    permute_order:   the usual
//    ninners,inners,ntiles,tiles: the usual. But note that loops is a subset
//                     of inners and tiles.  That is, if it's not in loops,
//                     then even if it's in inners or tiles it doesn't go
//                     in this loop.
//    unrolls[d]:      how much loop unrolling was done for loop d.
//                     Note that arl_stripsz[d]*unrolls[d] is how many
//                     iterations are already in the block for this loop,
//                     so if we compute we want B iterations at this level,
//                     it really B*arl_stripsz[d]*unrolls[d] actual
//                     iterations.
//    iters_inside:    number of iterations inside a single block.  So
//                     if i is unrolled 3 times, it contributes a factor
//                     of 3.  If the strip size for i is 12 for the L1
//                     cache, then iters_inside is 12 (12 is a multiple of
//                     3, of course).
//    mhd_level:       possibly not used
//    required_blocksize[loopno]: 0 means unstripped, positive the strip amount
//    cache_fits_outside:    Does cache hold entire SNL?  If so, don't model
//                           cache, but model TLB.  (Both cache_fits_outside
//                           and tlb_fits_outside can't be true.)
//    tlb_fits_outside:      Does tlb hold entire SNL?  If so, don't model
//                           tlb, but model cache.  (Both cache_fits_outside
//                           and tlb_fits_outside can't be true.)
//    machine_cycles:        the usual
//
// return values:
//
//    c_cpi:            per iteration of the untransformed nest, how
//                           many cache cycles this cache level requires
//                           after this transformation.
//    o_cpi:            per iteration of the untransformed nest,
//                              how many cycles of loop overhead the proposed
//                              loops introduces.
//    bsz[i]: the blocksize corresponding to loops[i].  Note that we
//            return B*arl_stripsz[d]*unrolls[d].
//
// Philosophy:
// 
// The nest model is only valid when the data overflows the memory hierarchy
// level.  That's because if we have a loop like
//
//           for i
//             for j = 1,100
//               a(i,j)
//
// If the cache can hold 100 elements, then locality exists in the i loop
// even when we don't block.  So that's why One_Cache_Model checks first to
// see if the entire nest fits in the cache; if it does, we assign a cost
// of zero.  So here we know that if we consider all the loops in the nest,
// we'll overflow the cache.  And if these nests we're considering blocking
// all fit in the cache, then the model is incorrect, so we have to include
// the next loop out as well.
//
// In the case of a two deep loop, it's adequate to just return "not modelled"
// when a single loop fits in the cache, so long as we know that Nest_Model()
// will be called with both loops.  That's because when modelling both loops,
// that modelling will chose not blocking if that's the best choice.  In a
// three deep loop nest, if we want to consider
//
//       do i          do i
//        do j      =>  do k'
//         do k          do j
//                        do k
//
// we can't get that by calling Nest_Model() with 3-deep loop.  That's because
// when the inner two fit in the model, we can accurately model the blocking
// above by just modelling do i,j,k.  (That's because the inner two fit in
// the cache, so whatever you do in there doesn't matter.)  Nest_Model()
// on a three deep loop will choose that if that's best.
//
// But there's a problem.  The TLB and cache interact.  We might consider
//
//       do i          do i
//        do j      =>  do k'
//         do k          do j
//                        do k
//
// for the TLB.  But if the above fits in the *cache* (not the TLB), then we
// must model do i,j,k for the cache.  So it doesn't really help is to
// re-call Nest_Model() with more loops, and it doesn't help us to return
// no-answer, counting in further calls to Nest_Model to handle the modelling.
// So what we do instead is have nest_model itself add more and more loops
// until it can reach an answer.

static void Nest_Model(const ARRAY_REF*        arl,
                       INT                     nloops,
                       const INT*              loops,
                       const mINT64*           est_iters,
                       const mINT64*           max_iters,
                       const INT*              arl_stripsz,
                       INT                     depth,
		       INT 		       ninners, 
		       INT 		       ntiles, 
                       INT                     stripdepth,
                       const INT*              permute_order,
                       const INT*              available_order,
                       const INT               available_depth,
                       const INT*              unrolls,
		       INT                     iters_inside,
                       INT                     mhd_level,
                       const INT*              required_blocksize,
                       BOOL                    cache_fits_outside,
                       BOOL                    tlb_fits_outside,
                       double                  machine_cycles,
                       double*                 c_cpi,
                       double*                 o_cpi,
                       INT*                    bsz, 
		       const BOOL*             originally_has_reuse,
		       INT64 		       u_nest_number)
{
  // Our cache model charges a penalty for each cache miss.  This is correct,
  // because in One_Cache_Model, if things fit in the cache, we set
  // cache_fits_outside and skip this code.  So this code only executes
  // if we know that things don't fit.  But if there is outer reuse and
  // if the loop nest of interest (loops we want to tile within the SNL)
  // fit in the cache, then our model is wrong.  Luckily, we will just
  // model this nest with one extra loop in the nest.

  if (Debug_Cache_Model) {
    fprintf(TFile, "\n+++++ NEST MODEL "); 
    fprintf(TFile, "{"); 
    INT i;
    for (i = 0; i < nloops - 1; i++) 
      fprintf(TFile, "%d,", loops[i]);
    fprintf(TFile, "%d}: #%lld ", loops[nloops - 1], u_nest_number); 
    fprintf(TFile, "+++++\n"); 
  } 

  Is_True(!(cache_fits_outside && tlb_fits_outside),
          ("Nest_Model() shouldn't have these conditions--answer suboptimal"));

  double    rcache_const = 0.0;
  double    wcache_const = 0.0;
  double    rtlb_const = 0.0;
  double    wtlb_const = 0.0;

  FORMULA*  tlb_initialization = NULL;
  FORMULA*  cache_initialization = NULL;

  INT       rcache_sreg = 1;
  INT       wcache_sreg = 2;
  INT       ccpi_hidable_sreg = 3;
  INT       ccpi_nonhidable_sreg = 4;
  INT       rtlb_sreg = 5;
  INT       wtlb_sreg = 6;
  INT       tcpi_sreg = 7;
  INT       first_free_sreg = 10;

  double    cache_clean_cpb =
    double(Cur_Mhd->Clean_Miss_Penalty)/Cur_Mhd->Line_Size;
  double    cache_dirty_cpb =
    double(Cur_Mhd->Dirty_Miss_Penalty)/Cur_Mhd->Line_Size;
  double    cache_dirty_cpb_nonhidable =
    (cache_dirty_cpb-cache_clean_cpb)*
    Cur_Mhd->Pct_Excess_Writes_Nonhidable/100.0;
  double    cache_dirty_cpb_hidable =
    cache_dirty_cpb - cache_dirty_cpb_nonhidable;
  double    tlb_clean_cpb =
    double(Cur_Mhd->TLB_Clean_Miss_Penalty)/Cur_Mhd->Page_Size;
  double    tlb_dirty_cpb =
    double(Cur_Mhd->TLB_Dirty_Miss_Penalty)/Cur_Mhd->Page_Size;

  INT64 v1 = Estimate_Middle_Iters(loops[0], nloops, est_iters, 
                                   max_iters, arl_stripsz);

  if (Debug_Cache_Model) 
    fprintf(TFile, "USING MIDDLE LOOP ESTIMATE OF %lld ITERATIONS\n", v1); 

  if (!cache_fits_outside)
    cache_initialization = Compute_Actual_Miss_Bytes(
      arl, nloops, loops, est_iters, max_iters, unrolls, arl_stripsz,
      depth, ninners, ntiles, stripdepth, permute_order,
      available_order, available_depth, mhd_level, v1,
      FALSE, originally_has_reuse, rcache_sreg, wcache_sreg, first_free_sreg,
      &rcache_const, &wcache_const);

  if (!tlb_fits_outside)
    tlb_initialization = Compute_Actual_Miss_Bytes(
      arl, nloops, loops, est_iters, max_iters, unrolls, arl_stripsz,
      depth, ninners, ntiles, stripdepth, permute_order,
      available_order, available_depth,  mhd_level, v1,
      TRUE, originally_has_reuse, rtlb_sreg, wtlb_sreg, first_free_sreg,
      &rtlb_const, &wtlb_const);

  // now get a formula for loop overhead.

  // If this is changed, change then and else part.
  // The following model we are not using.  It assumes that we overlap
  // Cur_Mhd->Load_Op_Overlap of the cache cycles with machine cycles,
  // but only up to the number of machine cycles.  That would make sense,
  // and result in.
  //
  //   cache misses per iter = 
  //       cache_cpi < machine_cycles ? 
  //         (1.0 - Load_Op_Overlap) * cache_cpi :
  //         (1.0 - Load_Op_Overlap) * machine_cycles +
  //               (cache_cpi - machine_cycles)
  //
  // But we actually assume that the first cache cycles are overlapped at that
  // rate, but we linearly overlap less and less until finally the cache
  // cycle that overlaps with the last machine cycle only overlaps with the
  // the last machine cycle at half (or whatever) that rate.  After that,
  // then the cache
  // cycles count the full amount.  So the variable "counts0" is the proportion
  // of the cache cycle we include in the final answer, at first.  "countsm"
  // is the same thing for the cycle occuring with the last machine cycle.

  double    counts0 = 1.0 - Cur_Mhd->Load_Op_Overlap_1;
  double    countsm = 1.0 - Cur_Mhd->Load_Op_Overlap_2;

  if (nloops == 1) {
    FmtAssert(cache_initialization == NULL && tlb_initialization == NULL,
              ("Broken Compute_Actual_Miss_Penalty()"));

    double overhead_const = double(Loop_Overhead) / (v1 * iters_inside);
    double cache_cpi_const_hidable = 0.0;
    double cache_cpi_const_nonhidable = 0.0;
    double tlb_cpi_const = 0.0;

    if (rcache_const || wcache_const) {
      cache_cpi_const_hidable =
        (rcache_const * cache_clean_cpb +
         wcache_const * cache_dirty_cpb_hidable) /
           (Cur_Mhd->Typical_Outstanding * iters_inside);
      cache_cpi_const_nonhidable =
        (wcache_const * cache_dirty_cpb_nonhidable) /
           (Cur_Mhd->Typical_Outstanding * iters_inside);
    }
    if (rtlb_const || wtlb_const) {
      tlb_cpi_const = (rtlb_const * tlb_clean_cpb +
                       wtlb_const * tlb_dirty_cpb) / iters_inside;
      if (Mhd.TLB_NoBlocking_Model && machine_cycles) {
        double cache_cycles =
          cache_cpi_const_hidable + cache_cpi_const_nonhidable;
        if (cache_cycles < machine_cycles)
          // if approx equal, use full model.  If no cache cycles,
          // use none of tlb model
	  tlb_cpi_const *= cache_cycles / machine_cycles;
      }
    }
    if (cache_cpi_const_hidable < machine_cycles) {
      cache_cpi_const_hidable *= counts0 +
        (cache_cpi_const_hidable/machine_cycles)*(countsm - counts0)/2;
    }
    else {
      cache_cpi_const_hidable = (counts0 + countsm)/2 * machine_cycles + 
          (cache_cpi_const_hidable - machine_cycles);
    }
    *c_cpi = cache_cpi_const_hidable + cache_cpi_const_nonhidable +
                                 Mhd.TLB_Trustworthiness/100.0*tlb_cpi_const;
    *o_cpi = overhead_const;
    bsz[0] = 0;
  }
  else {
    FORMULA*  overhead = FORMULA::Const(double(Loop_Overhead) / v1);
    INT ii;
    for (ii = 0; ii < nloops - 1; ii++) {
      overhead = FORMULA::Div(FORMULA::Add(overhead,
                                           FORMULA::Const(Loop_Overhead)),
                              FORMULA::Var(ii));
    }

    // TODO OK: we're adding the overhead per iter and cache cycles per
    // iter, and minimizing that.  This may not be quite the best thing.
    // If there happens to be "extra memory cycles" (the bus isn't saturated)
    // then it may be better to pass such information down to this code.
    // We are not doing that.  This may have the effect of counting the cache
    // more than we should.

    if (cache_initialization == NULL) {
      FORMULA* rset = FORMULA::Set(rcache_sreg, FORMULA::Const(rcache_const));
      FORMULA* wset = FORMULA::Set(wcache_sreg, FORMULA::Const(wcache_const));
      cache_initialization = FORMULA::Comma(rset, wset);
    }
    if (tlb_initialization == NULL) {
      FORMULA* rset = FORMULA::Set(rtlb_sreg, FORMULA::Const(rtlb_const));
      FORMULA* wset = FORMULA::Set(wtlb_sreg, FORMULA::Const(wtlb_const));
      tlb_initialization = FORMULA::Comma(rset, wset);
    }

    FORMULA* rcache_cycles = FORMULA::Mul(FORMULA::Use(rcache_sreg),
                                          FORMULA::Const(cache_clean_cpb));
    FORMULA* wcache_cycles_hidable =
      FORMULA::Mul(FORMULA::Use(wcache_sreg),
                   FORMULA::Const(cache_dirty_cpb_hidable));
    FORMULA* wcache_cycles_nonhidable =
      FORMULA::Mul(FORMULA::Use(wcache_sreg),
                   FORMULA::Const(cache_dirty_cpb_nonhidable));
    FORMULA* cache_cpi_hidable = FORMULA::Add(rcache_cycles,
                                              wcache_cycles_hidable);
    FORMULA* cache_cpi_nonhidable = wcache_cycles_nonhidable;

    FORMULA* tlb_cpi =
      FORMULA::Add(FORMULA::Mul(FORMULA::Use(rtlb_sreg),
                FORMULA::Const(Mhd.TLB_Trustworthiness/100.0*tlb_clean_cpb)),
                   FORMULA::Mul(FORMULA::Use(wtlb_sreg),
                FORMULA::Const(Mhd.TLB_Trustworthiness/100.0*tlb_dirty_cpb)));

    if (Cur_Mhd->Typical_Outstanding > 1.0) {
      cache_cpi_hidable = FORMULA::Div(cache_cpi_hidable,
                               FORMULA::Const(Cur_Mhd->Typical_Outstanding));
      cache_cpi_nonhidable = FORMULA::Div(cache_cpi_nonhidable,
                               FORMULA::Const(Cur_Mhd->Typical_Outstanding));
    }

    FORMULA* c_and_t_set = FORMULA::Comma5(
      cache_initialization,
      tlb_initialization,
      FORMULA::Set(ccpi_hidable_sreg, cache_cpi_hidable),
      FORMULA::Set(ccpi_nonhidable_sreg, cache_cpi_nonhidable),
      FORMULA::Set(tcpi_sreg, tlb_cpi));
    FORMULA* cpi_compute;

    double   mci = machine_cycles * iters_inside;

    if (Cur_Mhd->Load_Op_Overlap_1 < 0.00001) {
      // simple case: cache+tlb+overhead
      cpi_compute = FORMULA::Add(FORMULA::Add(FORMULA::Use(ccpi_hidable_sreg),
                                              FORMULA::Use(tcpi_sreg)),
                                 overhead);
    }
    else {
      // more complicated case: cache+tlb+overhead,

      FORMULA* ctest = FORMULA::Lt(FORMULA::Use(ccpi_hidable_sreg),
                                   FORMULA::Const(mci));

      // the lhs is for when the cache_cpi < machine cycles

      FORMULA* lhs_x = FORMULA::Div(FORMULA::Use(ccpi_hidable_sreg),
                                    FORMULA::Const(mci));
      FORMULA* lhs_prop = FORMULA::Add(FORMULA::Const(counts0),
                 FORMULA::Mul(lhs_x, FORMULA::Const((countsm - counts0)/2)));
      FORMULA* lhs = FORMULA::Mul(lhs_prop,
                                  FORMULA::Use(ccpi_hidable_sreg));
      lhs = FORMULA::Add(lhs, FORMULA::Use(tcpi_sreg));

      // the rhs is for when the cache_cpi >= machine cycles

      FORMULA* rhs = FORMULA::Mul(FORMULA::Const((counts0 + countsm)/2),
                                  FORMULA::Const(mci));
      FORMULA* rhs2 = FORMULA::Sub(FORMULA::Use(ccpi_hidable_sreg),
                                   FORMULA::Const(mci));

      rhs = FORMULA::Add(rhs, rhs2);
      rhs = FORMULA::Add(rhs, FORMULA::Use(tcpi_sreg));

      cpi_compute = FORMULA::Add(FORMULA::Cond(ctest, lhs, rhs), overhead);
    }

    cpi_compute = FORMULA::Add(cpi_compute,
                               FORMULA::Use(ccpi_nonhidable_sreg));

    FORMULA* total_cpi = FORMULA::Comma(c_and_t_set, cpi_compute);

    if (Debug_Cache_Model >= 3) {
      fprintf(TFile, "+++++ USING TOTAL CPI FORMULA: \n");
      total_cpi->Print(TFile);
      fprintf(TFile, "\n+++++ END TOTAL CPI FORMULA\n\n");
    }

    mINT64* adj_max_iters = (mINT64*) alloca(sizeof(mINT64)*(depth+1));
    INT i;
    for (i = 0; i <= depth; i++) {
      // don't explore beyond MAX_BLOCKSIZE total iterations
      adj_max_iters[i] = MIN(max_iters[i], MAX_BLOCKSIZE/unrolls[i]);
      if (arl_stripsz[i] > 1)
        adj_max_iters[i] = adj_max_iters[i] / arl_stripsz[i];
    }

    if (Debug_Cache_Model) 
      fprintf(TFile, "+++++ TRYING BLOCKSIZES \n"); 

    RSolve_Go(total_cpi, nloops-1, loops+1, adj_max_iters,
              unrolls, required_blocksize, bsz+1);

    if (Debug_Cache_Model) { 
      fprintf(TFile, " SELECTED BLOCKSIZES:");
      fprintf(TFile, "("); 
      for (i = 1; i < nloops; i++) { 
        fprintf(TFile, "%d", bsz[i]);
	if (i < nloops - 1) 
	  fprintf(TFile, ","); 
      } 
      fprintf(TFile, ")\n"); 
    } 
    if (Debug_Cache_Model) 
      fprintf(TFile, "+++++ END TRYING BLOCKSIZES\n\n"); 

    if (Debug_Cache_Model >= 2) {
      fprintf(TFile, "+++++ OVERHEAD CPI FORMULA: \n");
      overhead->Print(TFile);
      fprintf(TFile, "\n+++++ END OVERHEAD CPI FORMULA\n\n");
    }

    *o_cpi = overhead->Eval(nloops-1, bsz+1) / iters_inside;
    *c_cpi = total_cpi->Eval(nloops-1, bsz+1) / iters_inside - *o_cpi;

    // Remove strips with too large iteration counts.
    // For good loops, multiply out to get strip sizes in terms of
    // original loop.  bsz[i] really has bsz[i]*arl_stripsz*unrolls
    // iterations.  If that number exceeds MAX_BLOCKSIZE, then include
    // in the block but don't strip.  And if less but still huge, then
    // there may be LCM problems down the road.  So if, say 38397, we like
    // 30000 better.

    bsz[0] = -1;
    INT stripped_count = 0;
    for (i = 1; i < nloops; i++) {
      INT ii = loops[i];
      if (bsz[i] >= adj_max_iters[ii])
        bsz[i] = 0;
      else {
        INT ssz = arl_stripsz[ii] > 1 ? arl_stripsz[ii] : 1;
        ssz *= unrolls[ii];
        FmtAssert(MAX_LCM % ssz == 0, ("Impossible block size"));
        INT new_lcm = MAX_LCM / ssz;
        INT new_bsz;
        for (new_bsz = bsz[i]; new_lcm % new_bsz; new_bsz--)
          continue;
        if (new_bsz > 1) {
	  bsz[i] = new_bsz*ssz;
	  stripped_count++;
        } else {
	  bsz[i] = 0;
        }
      }
    }
    if (stripped_count == 0)              // no tiling after all
      bsz[0] = 0;
  }

  if (Debug_Cache_Model) {
    fprintf(TFile, "++++ RESULTS FOR NEST MODEL #%lld\n", u_nest_number); 
    fprintf(TFile, "  Cache    %.4g cpi\n", *c_cpi); 
    fprintf(TFile, "  Overhead %.4g cpi\n", *o_cpi);
    fprintf(TFile, "  Total    %.4g cpi\n", *c_cpi + *o_cpi); 
    fprintf(TFile, "  Iterations Inside %d\n", iters_inside); 
    if (nloops > 1) {
      fprintf(TFile, "  Blocksizes = ");
      fprintf(TFile, "  (");
      INT i;
      for (i = 1; i < nloops; i++) { 
        fprintf(TFile, "%d", bsz[i]);
	if (i < nloops - 1)
	  fprintf(TFile, ","); 
      } 
      fprintf(TFile, ")\n");
    }
    fprintf(TFile, "++++ END RESULTS FOR NEST MODEL\n\n"); 
  }

  if (Debug_Cache_Model) { 
    fprintf(TFile, "+++++ END NEST MODEL "); 
    fprintf(TFile, "{"); 
    INT i;
    for (i = 0; i < nloops - 1; i++) 
      fprintf(TFile, "%d,", loops[i]);
    fprintf(TFile, "%d}: ", loops[nloops - 1]); 
    fprintf(TFile, "#%lld +++++\n\n", u_nest_number); 
  } 
}

typedef HASH_TABLE<const ARRAY_REF_NODE*,INT>	CONST_TAB;

//-------------------------------------------------------------------
// Has_Reuse:
//      arl:           the usual
//      has_reuse:     the output
//      first,depth:   the loops we are considering are [first..depth]
// TODO: Note that a(64*i,j) should not be considered to have reuse in i.
// Or should it?  It possibly should for the TLB but not the cache.
// For now, we conservatively assume it has reuse, but perhaps we should
// have one reuse vector each for the cache and TLB.
//-------------------------------------------------------------------
// This is a heuristic, used for pruning.
// Conservative is assuming everything has reuse.
// The algorithm.
//   Initialize with everything doesn't have reuse, no indices used.
//   If it finishes with never used, then has_reuse will be true.
//   Start with self-reuse.  Any time there's a missing index in an
//   array expression, we know there's reuse in that loop.  e.g. a(i)
//   has reuse in j.  There's nothing more we need to know about k.
//   Note that we recognize line reuse, so a(i) has reuse in i.
//   (If we have a(n,j) where n varies in i, then indexes i and out are
//   considered to be used, and don't get any reuse credit.)
//   Then group locality.  If still no reuse in a given loop, but
//   it's used, then try looking for a(i,j) and a(i,j+1) to spot j reuse.
//-------------------------------------------------------------------

static void Has_Reuse(const ARRAY_REF* arl,
		      const INT* outertiles,  
		      BOOL* has_reuse, 
		      INT first, 
		      INT depth)
{
  BOOL* used = (BOOL*) alloca(sizeof(BOOL)*(depth+1));
  INT i;
  for (i = first; i <= depth; i++) {
    has_reuse[i] = FALSE;
    used[i] = FALSE;            // if never used, shouldn't go in the tile
                                // since we get reuse anyway.
  }

  MEM_POOL_Push(&LNO_local_pool);

  CONST_TAB* const_tab = 
    CXX_NEW(CONST_TAB(103, &LNO_local_pool), &LNO_local_pool);

  // Start with self-reuse.
  //
  // A reference like a(i) has reuse in i and everything else.
  // So does a(<not i>, i) if the first dimension is less than
  // the line size.  But a(n*i) where n is a constant >= the happy coefficient
  // or n >= the first dimension #elements.
  INT ix;
  for (ix = 0; ix < arl->Elements(); ix++) {
    ARRAY_REF_CONST_ITER iter(arl->Array_Ref_List(ix));
    for (const ARRAY_REF_NODE* n = iter.First();
         !iter.Is_Empty(); n = iter.Next()) {
      ACCESS_ARRAY* aa = n->Array;
      WN*           wn = n->Wn;

      if (aa->Too_Messy)
        continue;

      // see which indices are ever used

      BOOL constant = TRUE;
      INT j;
      for (j = aa->Num_Vec()-1; j >= 0; j--) {
        ACCESS_VECTOR* av = aa->Dim(j);
	INT            ncl = av->Non_Const_Loops();
        for (INT ii = first; ii <= depth; ii++) {
          if (av->Loop_Coeff(ii) || ii < ncl) {
            used[ii] = TRUE;
            constant = FALSE;
          }
        }
      }
      if (constant) {
        const_tab->Enter(n,1);
        continue;
      }

      // anything not used (except possibly in stride one dim) has reuse.

      BOOL normal = wn && aa->Num_Vec() == WN_num_dim(wn);
      INT i;
      for (i = first; i <= depth; i++) {
        if (has_reuse[i] || outertiles[i])
          continue;

        INT j = aa->Num_Vec()-2;

        // see if the 2nd to last has cache line reuse too
        // 2nd to last kind of always has reuse when the "edge effects"
        // model is in place.

        if (normal) {
	  if (LNO_Cache_Model_Edge_Effects)
	    j--;
	  else {
            WN* wn1 = WN_array_dim(wn, WN_num_dim(wn)-1);
            INT linesz = Cur_Mhd->TLB_Valid() ? Cur_Mhd->Page_Size :
                                                Cur_Mhd->Line_Size;
            if (WN_operator(wn1) == OPR_INTCONST &&
                n->Element_Size() * WN_const_val(wn1) < linesz)
              j--;
	  }
        }
	INT jj;
        for (jj = j ; jj >= 0; jj--) {
          if (aa->Dim(jj)->Loop_Coeff(i) ||
	      i < aa->Dim(jj)->Non_Const_Loops()) {
	    if (used[i] == FALSE) {
	      DevWarn("Has_Locality has bug (self-corrected)");
	      used[i] = TRUE;
	    }
            break;
	  }
        }
        if (jj < 0) { // if index just not used (outside stride one indxs)
          // like we said, make sure that the low stride dimensions do not
          // have too-large coefficients:
	  BOOL reuse = TRUE;
	  if (j >= 0) {
	    INT jj;
            for (jj = aa->Num_Vec()-1; reuse && jj > j; jj--) {
              INT coeff = aa->Dim(jj)->Loop_Coeff(i);
              if (coeff > Happy_Coefficient)
                reuse = FALSE;
              else if (normal) {
                WN* wn1 = WN_array_dim(wn, WN_num_dim(wn)-1);
                if (WN_operator(wn1) == OPR_INTCONST &&
                    coeff >= WN_const_val(wn1))
                  reuse = FALSE;
              }
            }
          }
          has_reuse[i] = reuse;
        }
      }
    }
  }

  // Now handle the group a(i,j) and a(i,j+1) case.  A conservative check.
  // If two refs have the same base, and identical non-zero coeff for j
  // and a non-zero constant difference, assume j has group locality.
  // This is conservative but okay.

  for (i = first; i <= depth; i++) {
    if (used[i] == FALSE && !outertiles[i]) {
      has_reuse[i] = TRUE;
      continue;
    }
    if (has_reuse[i] || outertiles[i])
      continue;

    // okay, a pairwise search through the universe, to find a(i,j) and
    // a(i,j+1), which didn't have self-reuse but has group reuse
    INT ix;
    for (ix = 0; !has_reuse[i] && ix < arl->Elements(); ix++) {
      ARRAY_REF_CONST_ITER iter(arl->Array_Ref_List(ix));
      for (const ARRAY_REF_NODE* n = iter.First();
           !has_reuse[i] && !iter.Is_Empty(); n = iter.Next()) {
        ACCESS_ARRAY* aa = n->Array;
        if (aa->Too_Messy || const_tab->Find(n) || aa->Num_Vec() == 1)
          continue;

        BOOL seen = FALSE;
        ARRAY_REF_CONST_ITER iter2(arl->Array_Ref_List(ix));
        for (const ARRAY_REF_NODE* n2 = iter2.First();
             !iter2.Is_Empty(); n2 = iter2.Next()) {
          if (seen == FALSE) {
            if (n2 == n)
              seen = TRUE;
            continue;
          }

          ACCESS_ARRAY* aa2 = n2->Array;
          if (aa2->Too_Messy || const_tab->Find(n2) ||
              aa2->Num_Vec() != aa->Num_Vec())
            continue;
          INT j;
          for (j = aa->Num_Vec()-2; j >= 0; j--) {
            INT c1 = aa->Dim(j)->Loop_Coeff(i);
            INT c2 = aa2->Dim(j)->Loop_Coeff(i);
            INT cc1 = aa->Dim(j)->Const_Offset;
            INT cc2 = aa2->Dim(j)->Const_Offset;
            if (c1 != 0 && c1 == c2 && cc1 != cc2 && (cc1 - cc2)%c1 == 0)
              has_reuse[i] = TRUE;
          }
        }
      }
    }
  }

  if (Debug_Cache_Model >= 3) {
    fprintf(TFile, "Loops having reuse <before pruning>: (");
    INT has_reuse_total = 0; 
    INT i;
    for (i = first; i <= depth; i++) 
      if (has_reuse[i]) 
        has_reuse_total++; 
    INT has_reuse_count = 0; 
    for (i = first; i <= depth; i++) {
      if (has_reuse[i]) { 
	fprintf(TFile, "%d", i);
	if (++has_reuse_count < has_reuse_total)
	  fprintf(TFile, ","); 
      } 
    } 
    fprintf(TFile, ")\n\n");
  }

  CXX_DELETE(const_tab, &LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

static INT64 Exact_Iteration_Count(WN* loop)
{
  INT64         rval = -1;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);

  if (dli->LB->Num_Vec() == 1 && dli->UB->Num_Vec() == 1 &&
      dli->Step->Is_Const() && dli->Step->Const_Offset) {
    INT stp = dli->Step->Const_Offset;
    ACCESS_VECTOR* lb = dli->LB->Dim(0);
    ACCESS_VECTOR* ub = dli->UB->Dim(0);

    ACCESS_VECTOR *av = Add(lb, ub, &LNO_local_pool);
    if (av->Is_Const()) {
      rval = stp > 0 ? av->Const_Offset+1 : -av->Const_Offset-1;
      if (rval < 0)
        rval = 0;
      if (stp < 0)
        stp = -stp;
      rval = (rval + stp - 1)/stp;
    }
    CXX_DELETE(av, &LNO_local_pool);
  }

  return rval;
}

//-----------------------------------------------------------------------
// NAME: Limit_Reused_Loops 
// FUNCTION: Limit the number of loops marked for reuse to MAX_TILE_LOOPS
//   by selecting those which have the best locality when used as the 
//   innermost loop.  
//-----------------------------------------------------------------------

#define MAX_TILE_LOOPS 4 

static void Limit_Reused_Loops(const ARRAY_REF* arl, 
                               const INT*       unrolls,
			       BOOL* has_reuse, 
			       INT first, 
			       INT depth, 
			       const INT64* est_iters,
                               const INT64* max_iters)
{
  // Nothing to do if only a few loops have reuse.   
  INT reuse_count = 0; 
  INT i;
  for (i = first; i <= depth; i++) 
    if (has_reuse[i])   
      reuse_count++;
  if (reuse_count <= MAX_TILE_LOOPS) 
    return;

  // Mark those indices which have reuse;  
  INT reuse_indices[MAX_DEPTH]; 
  for (i = 0; i < first; i++) 
    reuse_indices[i] = -1; 
  for (i = first; i <= depth; i++) 
    reuse_indices[i] = has_reuse[i] ? i : -1;  

  // Compute footprints for each loop which has reuse, pretending that 
  // each is the innermost loop.
  double footprint_sizes[MAX_DEPTH]; 
  for (i = 0; i < first; i++) 
    footprint_sizes[i] = (double) 0;  
  INT loops[1]; 
  INT arl_stripsz[MAX_DEPTH];
  for (i = 0; i <= depth; i++) {
    arl_stripsz[i] = 1; 
  }
  INT permute_order[MAX_DEPTH];  
  for (i = first; i <= depth; i++) {
    loops[0] = i;
    INT j;
    for (j = 0; j < first; j++) 
      permute_order[j] = j; 
    for (j = first; j < depth; j++) 
      permute_order[j] = j < i ? j : j + 1;
    permute_order[depth] = i; 
    COMPUTE_FOOTPRINT_RVAL Fmla = Compute_Footprint(arl, 1, loops, arl_stripsz,
      est_iters, max_iters, unrolls, depth, depth + 1, permute_order,
      -1, est_iters[i], FALSE, -1);
    double footprint = Fmla.AllFormula() == NULL ? 0 :
                       Fmla.AllFormula()->Eval(0, (const double *) NULL);
    footprint /= est_iters[i];  
    footprint_sizes[i] = MAX(footprint, 0.0);
  } 

  // Sort the results, smallest footprints first. 
  INT j = 0; 
  for (i = first; i <= depth; i++) {  
    if (reuse_indices[i] >= 0) { 
      reuse_indices[j] = reuse_indices[i]; 
      footprint_sizes[j++] = footprint_sizes[i]; 
    } 
  } 
  FmtAssert(j == reuse_count, ("Mistake counting reused loops."));
  for (i = 0; i < MAX_TILE_LOOPS; i++) {  
    for (j = i + 1; j < reuse_count; j++) {  
      if (footprint_sizes[j] < footprint_sizes[i]) { 
	INT reuse_index = reuse_indices[i]; 
        reuse_indices[i] = reuse_indices[j];
	reuse_indices[j] = reuse_index; 
	double footprint_size = footprint_sizes[i]; 
	footprint_sizes[i] = footprint_sizes[j]; 
	footprint_sizes[j] = footprint_size; 
      } 
    } 
  }
  for (i = MAX_TILE_LOOPS + 1; i <= depth; i++) { 
    reuse_indices[i] = -1; 
    footprint_sizes[i] = 0.0; 
  }  

  // Mark only the lowest MAX_TILE_LOOPS indices 
  for (i = first; i <= depth; i++) 
    has_reuse[i] = FALSE; 
  for (i = 0; i < MAX_TILE_LOOPS; i++) 
    has_reuse[reuse_indices[i]] = TRUE; 
}   

// Given a the permutation order and information about the transformation,
// we construct the implicit ordering of the loops where inside "don't count".
// See comments at top of file for philosophy.  But, for example,
// do q do i'' do k' do l' do i do j do k do l do m
// The one before the first duplicate is the middle loop.  Exception, the
// if no strips are duplicates, the loop just inside the strip is the middle
// loop.  If no strips, then call the innermost loop the middle loop.
// Return available_depth, which is one less than the number of components
// in available_order.

static INT Update_Available_Order(INT        depth,
                                  const INT* permute_order,
                                  INT        nstrips,
                                  INT        stripdepth,
                                  const INT* iloop,
                                  INT*       available_order)
{
  Is_True(Is_Permutation_Vector(permute_order, depth+1),
          ("Not a permutation"));

  if (nstrips == 0)
    stripdepth = depth;  // value only in this function

  Is_True((nstrips == 0) == (stripdepth == depth), ("Bad stripdepth"));

  // place through stripdepth
  INT i;
  for (i = 0; i < stripdepth; i++)
    available_order[i] = permute_order[i];

  if (stripdepth > depth)
    return depth;

  // place strips where we have not seen the corresponding loop.  When
  // we have seen, then we are done.
  INT available_depth = stripdepth-1;            // next loop to place
  INT s;
  for (s = 0; s < nstrips; s++) {
    if (Is_In_Array(iloop[s], available_order, available_depth+1))
       return available_depth;
     else
       available_order[++available_depth] = iloop[s];
  }
  
  // The only other loop to place is the middle loop

  INT middle_loop = permute_order[stripdepth];
  if (!Is_In_Array(middle_loop, available_order, available_depth+1))
    available_order[++available_depth] = middle_loop;

#ifdef Is_True_On
  for (i = 0; i <= available_depth; i++) 
    FmtAssert(available_order[i] >= 0 && available_order[i] <= depth, 
      ("Bad available order entry")); 
#endif
  return available_depth;
}

// One_Cache_Model:  Evaluate a given inner loop for a single level of
//                   the cache hierarchy.  Some blocking may have already
//                   been done for a cache further in.
//
//    arl:           References in the nest.
//    depth:         Depth of innermost loop.  E.g. in
//                     DO i
//                      DO j
//                       DO k'
//                       DO k
//                        DO l
//                    where the loop nest of interest is (i,j,k,l) with
//                    SNL of (k,l), 'depth' is the depth of l, ie 3.
//    stripdepth:     This is an in/out scalar.  All strips are placed
//                    outside this loop.
//    nstrips:        in/out scalar.  How many strips.
//    iloop:          in/out array.  iloop[0] holds the loop that is to be
//                    stripped to make the outermost of the strips.  Note
//                    that in other parts of the compiler, iloop[0] holds
//                    the loop number after permutation.  That's not the
//                    case here.
//    stripsz:        in/out array.  stripsz[0] is the strip size of the
//                    outermost strip.
//    striplevel:     in/out array.  the mhd_level for this strip, plus one.
//    permute_order:  in/out array.  As input, it's the order
//                    in which the loops occur.  In the above example, it's
//                    (0,1,2,3) or (0,1,3,2), depending upon whether we
//                    are exploring l or k innermost.  The output is the same,
//                    because One_Cache_Model never changes the inner loop.
//                    If the SNL were three deep, permute_order may change.
//                    E.g. if we have (i,j,k) and the model decides that only
//                    i should participate in a tile with k, then the permute
//                    order out will change to (j,i,k).  For multi-level
//                    blocking, we don't want to mess up what's been done
//                    before.  So we can only reorder loops outside the
//                    current stripdepth.
//    available_order:in/out array.  The loops available for blocking.  For the
//                    L1 cache, this is equivalent to permute order, but not
//                    after that.  For example, if L1 transforms "do i,j,k"
//                    into "do k',i,j,k", then j is fully blocked inside,
//                    and i is the innermost loop as far as L2 is concerned,
//                    so available order is not {i,j,k} but just {k,i}.
//    available_depth:in/out scalar.  A pointer to the number of entries
//                    minus one in available_order. (Also the depth of the new 
//		      innermost loop for this level of the cache model.) 
//    inners, ninners: in array and scalar.  The loop numbers (in no
//                    particular order) for loops that can be blocked
// 		      and are part of the fully permutable set. 
//    tiles, ntiles:  in array and scalar.  The loop numbers (in no
//                    particular order) for loops that can be blocked,
//                    but only assuming all in ninners are already blocked 
// 		      and are inside these loops.  The sets inners[] and 
//		      tiles[] are mutually exclusive.  The loops in tiles[]
//		      will not have their order changed.  
//    unrolls:        in array.  unrolls[0] says how much loop 0 has been
//                    outer unrolled.
//    est_iters:      how many times the loop will go, approximately.  Note
//                    that if it's i=1,100 but 16 unrolls, then answer is 7.
//                    This is for the original nest, before any transformation.
//    max_iters:      same as est iters if exact.  Otherwise bigger if a
//                    triangular loop and UNBOUNDED_ITERS if symbolic.
//                    This is for the original nest, before any transformation.
//    blocking_disabled: do not apply blocking.  (Basically, just compute
//                    costs for this inner loop.)
//    required_blocksize[loopno].  All loop in
//                    inners or tiles that have a >= 0 value must be
//                    included in the tile.  And they must get the specified
//                    blocking size.  Except that if it's not possible (e.g.
//                    an unroll of 2 and size of 5, or a size of 1), then
//                    just approximate.
//    mhd_level:      which cache we are working on.  -1 means no cache:
//                    just compute overheads.
//    machine_cycles: cpi for loop, ignoring cache misses
//  results:
//    cycles_per_iter:out scalar: how many cycles per iteration for
//			memory and loop overhead.  Always >=0.
//    doverhead_best  do loop overhead only.  Note that in this case
//                    and for cycles_per_iter, the loop overhead is
//                    for the entire nest, even if blocking is done
//                    for a different level.
//
// Note: iloop fills with the original loop number.  The definition
// used in other parts of the compiler is the loop number after
// permutation.

static void One_Cache_Model(const ARRAY_REF*   arl,
                            INT                depth,
                            INT*               stripdepth,
                            INT*               nstrips,
                            INT*               iloop,
                            INT*               stripsz,
                            INT*               striplevel,
                            INT*               permute_order,
                            INT*               available_order,
                            INT*               available_depth,
                            const INT*         inners,
                            INT                ninners,
                            const INT*         tiles,
                            INT                ntiles,
                            const INT*         unrolls,
			    const INT* 	       outertiles, 
                            const INT64*       est_iters,
                            const INT64*       max_iters,
                            BOOL               blocking_disabled,
                            const INT*         required_blocksize,
                            INT                mhd_level,
                            double             machine_cycles,
			    double 	       old_doverhead_best, 
                            double*            cycles_per_iter,
                            double*            doverhead_best)
{
  INT i;
  INT s;
  MEM_POOL_Push(&LNO_local_pool);

#if Is_True_On
  {
    // input checks.

    // a loop may occur in inners[] or tiles[] at most once, and
    // avalable_order must be a superset of those two.

    INT counts[MAX_DEPTH+1];
    for (i = 0; i <= depth; i++)
      counts[i] = 0;
    for (i = 0; i < ninners; i++) {
      FmtAssert(inners[i] >= 0 && inners[i] <= depth, ("sanity check dies"));
      counts[inners[i]]++;
    }
    for (i = 0; i < ntiles; i++) {
      FmtAssert(tiles[i] >= 0 && tiles[i] <= depth, ("sanity check dies"));
      counts[tiles[i]]++;
    }
    for (i = 0; i <= depth; i++)
      FmtAssert(counts[i] == 0 || counts[i] == 1, ("sanity check dies"));
    for (i = 0; i <= *available_depth; i++)
      if (counts[available_order[i]])
        counts[available_order[i]]--;
    for (i = 0; i <= depth; i++)
      FmtAssert(counts[i] == 0, ("sanity check dies"));
  }
#endif

  if (Debug_Cache_Model) { 
    if (mhd_level == 0) 
      fprintf(stdout, "\n"); 
    fprintf(stdout, "Memory Level #%d. Required inner #%d.\n", 
      mhd_level, available_order[*available_depth]); 
  }

  Rtry_Count = 0;

  Happy_Coefficient = MAX(10, 3*LNO_Outer_Unroll);
  Happy_Coefficient = MAX(Happy_Coefficient, 3*LNO_Outer_Unroll_Max);
  Happy_Coefficient = MAX(Happy_Coefficient, 3*LNO_Outer_Unroll_Prod_Max);
  Happy_Coefficient = MIN(Happy_Coefficient, 30);

  Cur_Mhd = NULL;
  if (mhd_level >= 0 && mhd_level < MHD_MAX_LEVELS) {
    Cur_Mhd = &Mhd.L[mhd_level];
    if (!Cur_Mhd->Valid())
      Cur_Mhd = NULL;
  }

  if (Debug_Cache_Model) {
    fprintf(TFile,
      "\n*****  L[%d] CACHE MODEL FOR REQUIRED INNER LOOP %d  *****\n",
      mhd_level, available_order[*available_depth]);
  }

  // the arl_stripsz is the number of iterations of the loop executed
  // within the block.  E.g. if a block is stripped by 30 then the
  // arl_stripsz of that loop is 10 if the unrolling factor is 3.
  // For loops outside the stripdepth (i.e. not in the block), there is
  // obviously exactly one iteration, so arl_stripsz = 1.  Any loop not
  // in the available_order is fully within the block.  We set
  // arl_stripsize=0 for such a loop.

  INT* arl_stripsz = (INT*) alloca(sizeof(INT)*(depth+1));
  for (i = 0; i <= depth; i++) {
    // if it's in the available loops, default to 1, else 0.
    arl_stripsz[i] = 0;
    INT ii;
    for (ii = 0; ii <= *available_depth; ii++) {
      if (available_order[ii] == i) {
        arl_stripsz[i] = 1;
        // look for the biggest strip for i
        for (s = 0; s < *nstrips; s++) {
          if (iloop[s] == i) {
            FmtAssert(stripsz[s] % unrolls[i] == 0, ("impossible"));
            FmtAssert(stripsz[s] >= unrolls[i], ("impossible"));
            arl_stripsz[i] = stripsz[s] / unrolls[i];
            break;
          }
        }
        break;
      }
    }
  }

  // iters_inside is the number of iterations of the the original
  // (not unrolled) nest that occurs in the current block.  This only
  // includes one iteration of the "middle" loop since we are doing 2n-1
  // blocking.

  INT iters_inside = 1;
  INT d;
  for (d = 0; d <= depth; d++) {
    INT iters = arl_stripsz[d] ? arl_stripsz[d] : est_iters[d];

    iters_inside *= iters * unrolls[d];

    FmtAssert(iters && unrolls[d], ("iters, unrolls cannot possibly be zero"));
    FmtAssert(MAX_LCM % unrolls[d] == 0,
              ("Unrolling factor too large for cache model: %d", unrolls[d]));
  }

  UINT64	u_best = 0;
  double        dcache_best = 0.0;
  double        dovhd_best = 0.0;
  INT		uloops_best = 0;
  INT*          rsz_best = (INT*) alloca(sizeof(INT)*(depth+1));

  if (depth >= MAX_DEPTH || Cur_Mhd == NULL) {
    // only model loop overhead
    if (depth >= MAX_DEPTH)
      DevWarn("loop depth is %d >= %d -- can't cache model", depth, MAX_DEPTH);
    *cycles_per_iter = Compute_Do_Overhead(depth, *stripdepth, *nstrips, iloop,
      stripsz, unrolls, est_iters, permute_order);
    if (Debug_Cache_Model >= 2)
      fprintf(TFile,
              "***** END ONE CACHE MODEL ovhd-only ovhd = %g *****\n",
              *cycles_per_iter);
    *doverhead_best = *cycles_per_iter;
    goto one_cache_model_return_point; 
  }

  {
  
    MAT<FRAC>::Set_Default_Pool(&LNO_local_pool);
    FORMULA::Fpool = &LNO_local_pool;
  
    // Note: if Has_Reuse() returned that all loops had reuse, it would be
    // ok, since loop overhead modelling discourages excess blocking anyway.
    // This is here just to speed up the process.  It's very quick to get a
    // handle on which loops don't have basic reuse.
  
    BOOL  has_reuse[MAX_DEPTH];
    Has_Reuse(arl, outertiles, has_reuse, 0, depth);
    Limit_Reused_Loops(arl, unrolls, has_reuse, depth - ninners + 1, depth, 
      est_iters, max_iters);
  
    {
      // an efficiency hack
      INT i;
      for (i = 0; i <= depth; i++) {
        if (has_reuse[i] == TRUE)
          break;
      }
      if (i > depth)      // no reuse
        blocking_disabled = TRUE;
    }
  
    // the variable 'k' is a bit vector.  Each 0 bit do not to include the
    // given loop in the innermost tile and each bit of 1 says that that loop
    // is to be in the innermost tile.  Bit i (set iff k&(1<<i)) refers to
    // loops as follows.  Bits 0 through ninners-1 refer to the loop of depth
    // inners[i].  Bits ninners through ninners+ntiles-1 refer to the loop
    // of depth tiles[i-ninners].  This encoding allows very fast iteration
    // through the possible loops we can tile.  For example, stepping by one
    // from k=1 to k=(1<<ninners)-1 gives us all possible tilings of the legal
    // innermost loops (at least one loop, each combination visited once).

    // available_order[available_depth] must be innermost in this tile
    // when we are at the
    // lowest level of the memory hierarchy.  The rest of the time, the
    // transformation specification is slightly easier this way and it's so
    // rare this will cause a problem.
  
    INT    krequired_inner = -1;
  
    for (i = 0; i < ninners; i++) {
      if (inners[i] == available_order[*available_depth]) {
        FmtAssert(krequired_inner == -1, ("Impossible"));
        krequired_inner = i;
      }
    }
    if (krequired_inner == -1) {
      for (i = 0; i < ntiles; i++) {
        if (tiles[i] == available_order[*available_depth]) {
          FmtAssert(krequired_inner == -1, ("Impossible"));
          krequired_inner = i + ninners;
        }
      }
    }
    if (krequired_inner == -1) {
      // only model loop overhead
      DevWarn("Couldn't find required inner loop!");
      *cycles_per_iter = Compute_Do_Overhead(depth, *stripdepth, *nstrips, 
		iloop, stripsz, unrolls, est_iters, permute_order);
      if (Debug_Cache_Model >= 2)
        fprintf(TFile,
                "***** END ONE CACHE MODEL (missing inner) ovhd = %g *****\n",
                *cycles_per_iter);
      *doverhead_best = *cycles_per_iter;
      goto one_cache_model_return_point;
    }
  
    Is_True(Is_Permutation_Vector(permute_order,depth+1),("Bad permutation"));
  
    // Check if the SNL we are modelling fits in the cache.  If so, then 
    // return, since there won't be any need to block loops for the cache, 
    // and the current technique does not model this case correctly. 
  
    INT kk = 0; 
      INT snl_loops[MAX_DEPTH]; 
    for (i = *available_depth + 1 - ninners - ntiles;i<=*available_depth;i++) 
      snl_loops[kk++] = available_order[i];
    INT snl_nloops = ninners + ntiles; 
    FmtAssert(snl_nloops == kk, ("Indexing problem detected."));
    double cache_bytes = (double) 0.0;
    BOOL fits = Fits_In_The_Cache(arl, snl_loops, snl_nloops, est_iters, 
      max_iters, arl_stripsz, unrolls, depth, *stripdepth, permute_order, 
      FALSE, &cache_bytes);
    if (Debug_Cache_Model >= 3) {
      if (fits)
        fprintf(TFile, "FITS IN THE CACHE. %g BYTES\n\n", cache_bytes);
      else
        fprintf(TFile, "DOES NOT FIT IN THE CACHE. %g BYTES\n\n",
	  cache_bytes);
    } 
    double tlb_bytes = (double) 0.0;
    BOOL tlb_fits = Fits_In_The_Cache(arl, snl_loops, snl_nloops, est_iters, 
      max_iters, arl_stripsz, unrolls, depth, *stripdepth, permute_order, TRUE,
      &tlb_bytes);
    if (Debug_Cache_Model >= 3) {
      double pages = Cur_Mhd->TLB_Valid() 
        ? tlb_bytes/Cur_Mhd->Page_Size : tlb_bytes;
      if (tlb_fits)
        fprintf(TFile, "FITS IN THE TLB. %g PAGES\n\n", pages);
      else
        fprintf(TFile, "DOES NOT FIT IN THE TLB. %g PAGES\n\n", pages);
    } 
    if (fits && tlb_fits) { 
      *cycles_per_iter = Compute_Do_Overhead(depth, *stripdepth, *nstrips, 
                                 iloop, stripsz, unrolls, est_iters,
                                             permute_order);
      *doverhead_best = *cycles_per_iter;
      goto one_cache_model_return_point;
    } 
  
    // Nominal_Blocksize is a cheesy first guess as to what the blocksizes
    // will be for loops of different depths.  Speeds up the search by
    // choosing reasonable first guesses.
    {
      // 160 bytes per iter?  Rough, but doesn't matter too much.  Better to
      // overestimate this number, since accuracy more important for big loops.
      // Of course, we could probably pass in a better estimate.
  
      double cs = !fits ? Cur_Mhd->Effective_Size/160.0 :
                          Cur_Mhd->TLB_Entries * Cur_Mhd->Page_Size;
      cs /= iters_inside;
  
      // TODO (nenad, 03/22/99):
      // Nominal_Blocksizes DO matter a lot, because they are currently
      // used to estimate (4*) the number of middle loop iterations.
      // For a snigle-loop nest, middle loop estimate is always 4000.
      // For multiple-loop nests. L0 effective size is 5637, and thus
      // cs = 5637/160 = 35, which means that block sizes will be very
      // small for l >= 2. 
      //
      // At the very least we should make Nominal_Blocksize[1] be 
      // consistent with those assigned in the loop. We could also
      // make the adjustment when estimating middle loop iterations 
      // so that we favor (close to ) square loop nests.

      Nominal_Blocksize[0] = 1000;	// doesn't matter, does it?
      Nominal_Blocksize[1] = 500;	// shouldn't matter.
      INT l;
      for (l = 2; l <= ntiles+ninners; l++) {
        // one more than the maximum nloops
        double nblock = l == 2 ? cs : l == 3 ? sqrt(cs) : pow(cs, 1.0/(l-1));
        INT i;
        for (i = 1; i < 20; i++) {
          if (double(1 << i) > nblock)
            break;
        }
        Nominal_Blocksize[l] = MIN(1 << (i-1), (MAX_BLOCKSIZE>>1));
      }
    }
    
    INT           required_blocksize_mask = 0;
    if (!blocking_disabled) {
      for (i = 0; i < ninners; i++)
        if (required_blocksize[inners[i]] >= 0)
          required_blocksize_mask |= (1<<i);
      for (i = 0; i < ntiles; i++)
        if (required_blocksize[tiles[i]] >= 0)
          required_blocksize_mask |= (1<<(i+ninners));
    }
  
    BOOL          bestseen = FALSE;
    UINT64        ninners_mask = (UINT64(1) << ninners) - 1;
  
  
    UINT64	u = 0;                                  // which loops in tile
    INT		uloops = 0;                             // # loops in tile
    double        dcache = 0.0;                           // miss cycles/iter
    double        doverhead = 0.0;                        // loop overhead/iter
    INT*          rsz = (INT*) alloca(sizeof(INT)*(depth+1)); // block sizes
  
  
    for (UINT64 k = 1; k < (UINT64(1) << (ninners+ntiles)); k++) {
      if (k > ninners_mask) {
        // Includes a "tile loop": include all loops further in
        // But if this tile loop has no locality, then don't bother
        // modelling, just continue on, since it can't be better than
        // anything we've seen so far.
        INT kk; 
        for (kk = ninners+ntiles-1; kk >= ninners; kk--)
          if (k && (1 << kk))
	    break;
  
        k |= ((1 << kk) - 1);
  
        // this conditional is the same as in the else part, but the first
        // two clauses from the else part are tautologically true here.
        if (has_reuse[tiles[kk-ninners]] == FALSE &&
            required_blocksize_mask != (1<<krequired_inner) &&
            !(required_blocksize_mask & (1<<kk)))
          continue;
      }
      else {
        // There's no reason to include a loop without reuse in the innermost
        // tile except (1) because it enables more tiling, or (2) because
        // it must be innermost.  Case (1) is handled in the then-part above, 
        // so check here for case (2).
        INT kk; 
        for (kk = 0; kk < ninners; kk++)
          if ((k&(1<<kk)) &&                       // for loop kk
              inners[kk] != available_order[*available_depth] && 
						// that's not reqd innermost
              has_reuse[inners[kk]] == FALSE &&    // and doesn't have reuse
                                 // and isn't needed to block reqd innermost
              required_blocksize_mask != (1<<krequired_inner) &&
                                 // and isn't itself a loop we have to block
              !(required_blocksize_mask & (1<<kk)))
            break;
        if (kk < ninners)
        continue;
      }

      // if requested loop is not inner, then don't try this case.
      // if blocking is disabled and we block, then don't try this case.
      // if blocking less than requested, then don't try this case.
    
      if (!(k & (1<<krequired_inner)))
        continue;
  
        if (blocking_disabled && k != (1<<krequired_inner))
        continue;
        
        if ((required_blocksize_mask & k) != required_blocksize_mask)
        continue;
  
        // If multi-level blocking, and we are trying a loop that was
      // never stripped down below but included in a tile, don't waste
      // time even considering stripping it.  It's what L1 wants, and
      // it's change L1's answer, so there's probably little reason to
      // change this code.  But if you want to,
      // be careful when removing this restriction -- can code further
      // down handle arl_stripsz[i] = 0 well?  I *think* so. 
  
        for (i = 0; i <= depth; i++) {
        if (i != available_order[*available_depth] &&
            arl_stripsz[i] == 0 && k&(1<<i))
          break;
      }
      if (i <= depth)
        continue;
  
      // map the bit vector k to the actual loops in the nest, bit vector u.
  
      u = 0;		        // which loops participate in tile
      uloops = 0;		// the number of loops participating in tile
      INT kk; 
      for (kk = 0; kk < ninners+ntiles; kk++) {
        if (k & (UINT64(1)<<kk)) {
          if (kk < ninners)
            u |= (UINT64(1)<<inners[kk]);
          else
            u |= (UINT64(1)<<tiles[kk-ninners]);
          uloops++;
        }
      }
  
      // the new order within the tile: same as input tiled order,
      // but those actually in the tile go innermost.
  
      INT	lloops[MAX_DEPTH+1];
      INT	nnloops = 0;
      INT i; 
      for (i = 0; i <= *available_depth; i++) {
        INT ii = available_order[i];
        if (u & (UINT64(1) << ii))
          lloops[nnloops++] = ii;
      }
      FmtAssert(nnloops == uloops, ("Bug in cache_model.cxx"));
  
      Nest_Model(arl, nnloops, lloops, est_iters, max_iters,
        arl_stripsz, depth, ninners, ntiles, *stripdepth, permute_order, 
        available_order, *available_depth,
        unrolls, iters_inside, mhd_level, required_blocksize,
        fits, tlb_fits, machine_cycles,&dcache,&doverhead, rsz, has_reuse, u);
  
  
      if (!bestseen || (dcache_best+dovhd_best) > (dcache+doverhead)) {
        if (Debug_Cache_Model)
          fprintf(stdout, "*");
        bestseen = TRUE;
        dcache_best = dcache;
        dovhd_best = doverhead;
        uloops_best = uloops;
        u_best = u;
        INT i;
        for (i = 0; i < uloops_best; i++)
          rsz_best[i] = rsz[i];
      } else {
        if (Debug_Cache_Model)
          fprintf(stdout, " "); 
      }
      if (Debug_Cache_Model) { 
        fprintf(stdout, "{"); 
        for (i = 0; i < nnloops - 1; i++) 
	  fprintf(stdout, "%d,", lloops[i]);
        fprintf(stdout, "%d}:", lloops[nnloops - 1]); 
        for (i = 0; i < 2 * (ninners + ntiles - nnloops) + 1; i++) 
          fprintf(stdout, " "); 
        fprintf(stdout, "  Cache: %8.4g LpOver: %8.4g", dcache, doverhead);  
        INT last_block_index = 0; 
        for (i = 1; i < nnloops; i++) 
          if (rsz[i] != 0) 
            last_block_index = i; 
        if (last_block_index > 0) { 
	  fprintf(stdout, " Blk: ["); 
	  for (i = 1; i < nnloops; i++) {
	    if (rsz[i] != 0) {
	      fprintf(stdout, "%d=%d", lloops[i], rsz[i]);
	      if (i != last_block_index) 
	        fprintf(stdout, ","); 
	    } 
          } 
	  fprintf(stdout, "]\n"); 
        } else { 
	  fprintf(stdout, "\n"); 
        } 
      }
    }
  
    //------------------------------------------------------------------------
    // We have the transformation.  first, fill blocksize[i] with the block 
    // size for loop number i.  That's this transformation only.
    //------------------------------------------------------------------------
  
    // use blocksize of -1 to mean "not blocked but in the tile"
  
    INT  strips_this_level = 0;
    INT  lp = 0;
    INT* blocksize = (INT*) alloca(sizeof(INT)*depth);
    for (i = 0; i <= *available_depth; i++) {
      INT ii = available_order[i];
      if (u_best & (UINT64(1) << ii)) {
        INT blksz = rsz_best[lp++];
        if (blksz > 1) {
          blocksize[ii] = blksz;
          if (LNO_Blocking_Size)
            blocksize[ii] = MIN(blocksize[ii], LNO_Blocking_Size);
          strips_this_level++;
        }
        else
          blocksize[ii] = -1;
      }
      else
        blocksize[ii] = 0;
    }
  
    // Any loop that was not blocked before but is blocked now goes in.
    // If there are no strips but blocksize != 0, then it's requested certain
    // loops be innermost.  We do the permutation.  But then we do not
    // decrease stripdepth.  This basically means that we rearrange the loops
    // for L1, but if L2 wants something different, we let it win in this case.
    // Changing stripdepth would mean the loops inside were untouchable, which
    // makes sense for blocked loops but not as much sense for unblocked ones.
    // For example, if we decide that i must be inner unblocked, isn't it okay
    // for L2 to decide to block the inner loop?  This heuristic can go wrong,
    // but looks pretty good to me.
  
    Is_True(Is_Permutation_Vector(permute_order,depth+1),("not permutation"));
    INT  oldstripdepth = *stripdepth;
    for (i = *stripdepth-1; i >= 0; i--) {
      INT ii = permute_order[i];
      if (blocksize[ii] != 0) {   // blocked, or -1 (would be if enough iters)
        INT j;
        for (j = i; j < *stripdepth-1; j++)
          permute_order[j] = permute_order[j+1];
        permute_order[*stripdepth-1] = ii;
        (*stripdepth)--;
      }
    }
    if (strips_this_level == 0)
      *stripdepth = oldstripdepth;
    Is_True(Is_Permutation_Vector(permute_order,depth+1),("not permutation"));
  
    // NOTE: we are throwing away dovhd_best.  That value holds an 
    // approximation
    // of overhead introduced by a transformation, and that helped us choose
    // a transformation, but it's not precise.  Compute_Do_Overhead() is
    // precise.  Of course, that also computes overhead for *all* 
    // transformation
    // (e.g. L1 and L2) but that's okay.  It would be nice to change Nest_Model
    // to use Compute_Do_Overhead, but that's hard, since we'd need it to be
    // able to generate a FORMULA, and it's probably not worth the effort.
  
    *doverhead_best = Compute_Do_Overhead(depth, *stripdepth, *nstrips, 
                                          iloop, stripsz, unrolls, est_iters,
                                          permute_order);
    *cycles_per_iter = dcache_best + *doverhead_best;
  
    // update the strips information
  
    for (i = *available_depth; i >= 0; i--) {
      INT ii = available_order[i];
      if (blocksize[ii] > 0) {    // a strip
        for (s = *nstrips - 1; s >= 0; s--) {
          iloop[s+1] = iloop[s];
          stripsz[s+1] = stripsz[s];
          striplevel[s+1] = striplevel[s];
        }
        iloop[0] = ii;
        stripsz[0] = blocksize[ii];
        striplevel[0] = mhd_level + 1;
        (*nstrips)++;
      }
    }
  
    *available_depth = Update_Available_Order(depth, permute_order, *nstrips,
      *stripdepth, iloop, available_order);
  
    if (u_best == 0) {
      // TODO OK: when specifying no blocking, no permutation, or some
      // pragmas, this may occur.  That's not good, but if things need
      // to be rewritten eventually anyway, we'll fix it then.  This also
      // can occur in other cases.  For example, if the dependences are
      // such that we can only block certain loops, then when Nest_Model
      // returns without evaluation (assuming more loops outside will be
      // be blocked) we get this situation.  It's not so clear how to
      // resolve it, but this is as good as anything.
  
      *cycles_per_iter = old_doverhead_best; 
      *doverhead_best = *cycles_per_iter;
    } 
  
  }  
one_cache_model_return_point: 

  if (Debug_Cache_Model) {
    fprintf(TFile, "+++++ RESULTS FOR L[%d] CACHE MODEL\n", mhd_level);
    fprintf(TFile, "  Required Inner Loop %d\n", 
      available_order[*available_depth]); 
    fprintf(TFile, "  Best Nest Model was {"); 
    INT i;
    for (i = 0; i <= depth; i++) {
      fprintf(TFile, "%d", permute_order[i]);  
      if (i < depth) 
	fprintf(TFile, ","); 
    } 
    fprintf(TFile, "}: #%lld\n", u_best); 
    fprintf(TFile, "  New Available Order is {"); 
    for (i = 0; i <= *available_depth; i++) {
      fprintf(TFile, "%d", available_order[i]);  
      if (i < depth) 
	fprintf(TFile, ","); 
    } 
    fprintf(TFile, "}\n"); 
    fprintf(TFile, "  Cache          %.4g cpi\n", dcache_best); 
    fprintf(TFile, "  Overhead       %.4g cpi\n", dovhd_best); 
    fprintf(TFile, "  Total          %.4g cpi\n", dcache_best + dovhd_best); 
    fprintf(TFile, "  True Overhead  %.4g cpi\n", *doverhead_best); 
    fprintf(TFile, "  True Total     %.4g cpi\n", *cycles_per_iter); 
    if (uloops_best > 1) {
      fprintf(TFile, "  Blocksizes =   ");
      fprintf(TFile, "(");
      INT i;
      for (i = 1; i < uloops_best; i++) {
        fprintf(TFile, "%d", rsz_best[i]);
	if (i < uloops_best - 1)
	  fprintf(TFile, ","); 
      } 
      fprintf(TFile, ")\n");
    }
    fprintf(TFile, "  Strips must be outside loop %d\n", *stripdepth);
    fprintf(TFile, "  List of stripped loops: \n"); 
    if (striplevel > 0) { 
      INT i;
      for (i = 0; i < *nstrips; i++)
	fprintf(TFile, "    Loop = %d, Strip Size = %d Strip Level = %d\n", 
	  iloop[i], stripsz[i], striplevel[i]); 
    } else { 
      for (INT i = 0; i < *nstrips; i++)
	fprintf(TFile, "    Loop = %d, Strip Size = %d\n", 
	  iloop[i], stripsz[i]);
    } 
    fprintf(TFile, "+++++ END RESULTS FOR L[%d] CACHE MODEL\n", mhd_level);
    fprintf(TFile,
      "\n*****  END L[%d] CACHE MODEL FOR REQUIRED INNER LOOP %d  *****\n\n",
      mhd_level, available_order[*available_depth]);
  }
  MEM_POOL_Pop(&LNO_local_pool);
}

// Block for smallest memory hierarcy level first.  That way, the right thing
// is done with small and medium problem size, and if the problem size is
// large, then the further blocking will help it too.  What we're trying to
// avoid is a case where, say, we block for the bigger cache and decide
// that's good enough for the smaller also.  While such a choice might even
// be good with a large size, our approach is more robust while delivering
// very good performance for all problem sizes whether N is known or not.
// In particular, our approach will not deliver significantly worse
// performace then if we had used the dumb approach of just blocking for
// the L1 cache only.

// Note iloop holds the actual loop number (as opposed to the loop number
// after permutation.  It changes it to the correct definition only as
// it's last action in Cache_Model.

void Cache_Model(ARRAY_REF*	arl,
                 INT		depth,
                 INT		required_inner,
                 const INT*	inners,
                 INT		ninners,
                 const INT*	tiles,
                 INT		ntiles,
                 const INT*	unrolls,
                 const DOLOOP_STACK*stack,
		 BOOL           blocking_disabled,
		 const INT*     required_blocksize,
                 double         machine_cycles,
		 INT            num_refs,

                 INT*		new_order,	// return array
                 INT*           nstrips,        // return val
                 INT*           stripdepth,     // return val
                 INT*           iloop,          // return array
                 INT*           stripsz,        // return array
                 INT*           striplevel,     // return array
                 double*        cycles_per_iter,// return scalar
		 double*        doverhead_best) // return scalar
{
  INT    i, s;
  INT outertiles[MAX_DEPTH]; 

  FmtAssert(new_order[depth] == -1 || required_inner == new_order[depth],
            ("Bad new_order/required_inner input"));

  if (LNO_Cache_Model == 1)
    Max_Different_Blocksizes = 1;

  if (Get_Trace(TP_LNOPT, TT_LNO_CACHE_MODEL_DEBUG))
    Debug_Cache_Model = CM_DEBUGGING_LEVEL;

  INT  mhd_level = (LNO_Cache_Model == 0) ? -1 : Mhd.First();
  INT* available_order = (INT*) alloca(sizeof(INT)*(depth+1));
  INT  available_depth = depth;

  new_order[depth] = required_inner;
  for (i = 0; i <= depth; i++) {
    BOOL already_ordered = FALSE;
    INT  first_free_slot = -1;
    INT j;
    for (j = 0; j <= depth; j++) {
      if (new_order[j] == i) {
        already_ordered = TRUE;
	break;
      }
      else if (first_free_slot == -1 && new_order[j] == -1)
        first_free_slot = j;
    }
    if (already_ordered == FALSE) {
      FmtAssert(first_free_slot >= 0, ("Impossible"));
      new_order[first_free_slot] = i;
    }
  }
  for (i = 0; i <= depth; i++) 
    available_order[i] = new_order[i];

  *nstrips = 0;
  *stripdepth = depth+1;

  Loop_Overhead = Mhd.Loop_Overhead_Base +
                  num_refs * Mhd.Loop_Overhead_Memref;

  if (Debug_Cache_Model) {
    fprintf(TFile, "*** CACHE MODEL (REQUIRED INNER LOOP=%d, ", required_inner); 
    fprintf(TFile, "LOOP OVERHEAD=%d) ***\n\n", Loop_Overhead); 
    Mhd.Print(TFile);
    if (Debug_Cache_Model >= 2) {
      fprintf(TFile, "ARRAY REFERENCE LIST:\n");
      arl->Print(TFile);
      fprintf(TFile, "END ARRAY REFERENCE LIST\n");
    }
  }

  mINT64* est_iters = (mINT64*) alloca(sizeof(mINT64)*(depth+1));
  mINT64* max_iters = (mINT64*) alloca(sizeof(mINT64)*(depth+1));

  for (i = 0; i <= depth; i++) {
    WN* wn_loop = stack->Bottom_nth(i);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
    outertiles[i] = dli->Is_Outer_Lego_Tile;  
    WN* wn_outer_tile = Outer_Tile(wn_loop, Du_Mgr);
    if (wn_outer_tile != NULL) {
      INT j;
      for (j = 0; j < i; j++) {
        WN* wn_tile_loop = stack->Bottom_nth(j);
        if (wn_tile_loop == wn_outer_tile)
	  break;
      }
      FmtAssert(j < i, ("Could not find tile loop"));
      outertiles[j] = TRUE; 
    }
  }
    
  for (i = 0; i <= depth; i++) {
    WN*           loop = stack->Bottom_nth(i);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
    // because of unrolling, this can be misleading.  Just check for -1 or not.
    INT64 exact_iters = Exact_Iteration_Count(loop);
    // Added this in for tomcatv and other benchmarks whose actual number
    // of iterations is given by the array bounds. 
    extern INT64 Get_Good_Num_Iters (DO_LOOP_INFO *dli);
    INT64 est = Get_Good_Num_Iters(dli);
    if (exact_iters != -1 && est != exact_iters) {
      DevWarn("Est_Num_Iterations=%lld, expected %lld",
              dli->Est_Num_Iterations, exact_iters);
      est = exact_iters;
    }
    est_iters[i] = (est + unrolls[i] - 1) / unrolls[i];
    Is_True(est_iters[i] >= 0, ("Bug: est_iters=%lld", est_iters[i]));
    if (est_iters[i] < 2)
      est_iters[i] = 1;

    INT64 max1 = dli->Est_Max_Iterations_Index == -1 ? UNBOUNDED_ITERS :
            (dli->Est_Max_Iterations_Index + unrolls[i] - 1) / unrolls[i];
    INT64 max2 = dli->Num_Iterations_Symbolic ? UNBOUNDED_ITERS :
            exact_iters != -1 ? est_iters[i] : est_iters[i]*2;
    INT64 max3 = dli->Is_Inner_Tile && dli->Tile_Size > 0 
	    ? dli->Tile_Size : UNBOUNDED_ITERS; 
    max_iters[i] = MIN(MIN(max1, max2), max3);
    if (max_iters[i] < 2)
      max_iters[i] = 1;
  }

  INT  total_unrolls = 1;
  INT* rqd_blksz = (INT*)alloca(sizeof(INT)*(depth+1));
  for (i = 0; i <= depth; i++) {
    total_unrolls *= unrolls[i];
    INT sz = required_blocksize[i*MHD_MAX_LEVELS + mhd_level];
    if (sz > 1) {
      INT iters_inside = unrolls[i];
      INT s;
      for (s = 0; s < *nstrips; s++) {
        if (iloop[s] == i) {
          iters_inside = stripsz[s];
	  break;
        }
      }
      rqd_blksz[i] = (sz + iters_inside - 1)/iters_inside;
    }
    else
      rqd_blksz[i] = sz;
  }
  
  One_Cache_Model(arl, depth, stripdepth, nstrips, iloop,
                  stripsz, striplevel, new_order,
                  available_order, &available_depth,
                  inners, ninners, tiles, ntiles,
                  unrolls, outertiles, est_iters, 
		  max_iters, blocking_disabled,
		  rqd_blksz, mhd_level, machine_cycles,
                  0.0, cycles_per_iter, doverhead_best);

  if (Debug_Cache_Model) {
    double cache_cycles = *cycles_per_iter - *doverhead_best; 
    double overhead_cycles = *doverhead_best; 
    fprintf(stdout, "  BEST:");  
    INT i;
    for (i = 0; i < 2 * (ninners + ntiles) - 3; i++) 
      fprintf(stdout, " "); 
    fprintf(stdout, "  Cache: %8.4g DoOver: %8.4g\n", cache_cycles, 
      overhead_cycles);
  }

  while ((mhd_level = Mhd.Next(mhd_level)) != -1) {
    // Multi-level blocking

    double n_cpi;
    double n_doverhead_best;
    INT    n_ninners;
    INT    n_ntiles;
    INT    n_inners[LNO_MAX_DO_LOOP_DEPTH];
    INT    n_tiles[LNO_MAX_DO_LOOP_DEPTH];

    // Compute which loops go in inners and tiles.  All inners loops must
    // be in this block for any tiles loops to be.  This isn't always
    // what we want, so the interface needs to be reworked.  Anyway,
    // Here's what we do.  Something can only be inner or tile if it's
    // available.  If it's available, all loops previously inner, plus
    // the new middle loop, are inner.  Finally, and this is important, 
    // if every previously tile loop is either stripped or newly inner
    // except one, then that loop is also inner.

    // start with previously tiled.  If still available, not new inner
    // and not stripped, put on this list.
    n_ntiles = 0;
    n_ninners = 0;
    INT i;
    for (i = available_depth; i >= 0; i--) { // get them in the right order
      if (i != available_order[available_depth] &&
	  Is_In_Array(i, tiles, ntiles) &&
          Is_In_Array(i, available_order, available_depth+1))
        n_tiles[n_ntiles++] = i;
    }
    // if all the inners have strips or are the new inner,
    // then this one tile is an inner also.
    if (n_ntiles == 1) {
      INT ii;
      for (ii = 0; ii < ninners; ii++) {
        if (inners[ii] != available_order[available_depth] &&
	    Is_In_Array(inners[ii], available_order, available_depth+1) &&
	    !Is_In_Array(inners[ii], iloop, *nstrips))
	  break;
      }
      if (ii >= ninners)
        n_ntiles = 0;
    }
    // anything available that was an inner or a tile and isn't a tile
    // is an inner.
    for (i = available_depth; i >= 0; i--) { // get them in the right order
      if (Is_In_Array(i, available_order, available_depth+1) &&
	  (Is_In_Array(i, tiles, ntiles) || Is_In_Array(i, inners, ninners)) &&
          !Is_In_Array(i, n_tiles, n_ntiles))
        n_inners[n_ninners++] = i;
    }

    for (i = 0; i <= depth; i++) {
      INT sz = required_blocksize[i*MHD_MAX_LEVELS + mhd_level];
      rqd_blksz[i] = sz > 1 ? (sz + unrolls[i] - 1)/unrolls[i] : sz;
    }

    One_Cache_Model(arl, depth, stripdepth, nstrips, iloop,
      stripsz, striplevel, new_order, available_order, &available_depth,
      n_inners, n_ninners, n_tiles, n_ntiles, unrolls, outertiles, 
      est_iters, max_iters, blocking_disabled, rqd_blksz, mhd_level,
      machine_cycles, *doverhead_best, &n_cpi, &n_doverhead_best);

    if (Debug_Cache_Model) {
      double overhead_cycles = n_doverhead_best - *doverhead_best;
      double cache_cycles = n_cpi - n_doverhead_best; 
      fprintf(stdout, "  BEST:");  
      INT i;
      for (i = 0; i < 2 * (ninners + ntiles) - 3; i++) 
	fprintf(stdout, " "); 
      fprintf(stdout, "  Cache: %8.4g DoOver: %8.4g\n", cache_cycles, 
        overhead_cycles);
    }

    // note this doverhead_best overrides the L1
    *cycles_per_iter += n_cpi - *doverhead_best;
    *doverhead_best = n_doverhead_best;

  }

  // iloop[s] holds the actual loop number.  But iloop[s]
  // is supposed to be such that after permutation it holds the loop
  // number.

  for (s = 0; s < *nstrips; s++) {
    INT i;
    for (i = 0; i <= depth; i++)
      if (new_order[i] == iloop[s])
        break;
    iloop[s] = i;
  }

#ifndef TARG_X8664
  // Ignoring bad reference bias heuristic to allow the right choice of inner loop
  // Only do this for X8664
  if (depth != required_inner && arl->Num_Bad()) {
    INT nbodies = 1;
    INT i;
    for (i = 0; i <= depth; i++)
      nbodies *= unrolls[i];
    double bias = BAD_REF_CYCLE_FIRST_BIAS +
                  (arl->Num_Bad()-1.0) * BAD_REF_CYCLE_INCREMENTAL_BIAS;
    if (Debug_Cache_Model) {
      fprintf(stdout, "Adding bad reference bias: %8.4g\n", bias / nbodies); 
    } 
    *cycles_per_iter += bias / nbodies;
  }
#endif /* TARG_X8664 */

  if (Debug_Cache_Model) {
    fprintf(TFile, "*** END CACHE MODEL (REQUIRED INNER LOOP=%d, ", 
      required_inner); 
    fprintf(TFile, "LOOP OVERHEAD=%d) ***\n\n", Loop_Overhead); 
  } 
}
