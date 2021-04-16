## Test environments
* local Windows install, R 4.0.5
* win-builder (release, devel and oldrelease)

## R CMD check results

R CMD check will produce the following false positive warning on older R 3.6.x releases:

> checking S3 generic/method consistency ... WARNING
  SelectedInput:
    function(...)
  SelectedInput.formula:
    function(..., data, control, metrics, stat, cutoff)

  See section 'Generic functions and methods' in the 'Writing R
  Extensions' manual.

This check issue has been fixed and does not occur in R 4.x.  See https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17701 for details.
