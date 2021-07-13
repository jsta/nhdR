## Test environments

* ubuntu 20 (on ghactions), R 4.1.0
* rhub (devel and release), including Solaris

## Have the issues why your package was archived been fixed? 

Yes, the prior issues only ocurred during certain tests on Solaris where the sf package did spatial projection operations (via the PROJ external library). These tests have been fixed by manually re-specifying the projection of internal package data. The fix follows https://stackoverflow.com/a/62268361/3362993

## R CMD check results

0 errors | 0 warnings | 0 note
