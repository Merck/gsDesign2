# gsDesign2 1.1.3

We are aware of the following NOTE, and we also maintain {simtrial}:

```
* checking dependencies in R code ... NOTE
Missing object imported by a ':::' call: ‘simtrial:::as_gt.simtrial_gs_wlr’
```


# gsDesign2 1.0.7

## Resubmission

This is a resubmission. In this version I have:

* Exported the internal functions to avoid the use of ::: in code examples.

* Fixed the write path issue by moving the test fixture generation script to data-raw/ which is not included in the package.
