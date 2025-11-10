* dropping NA values

. foreach var of varlist * {
  2.     quietly count if missing(`var')
  3.     if r(N) >= 857 {
  4.         drop `var'
  5.     }
  6. }


* Dropping high freq (> 85%)

foreach var of varlist * {
    quietly tab `var', matcell(freq)
    local max_freq = 0
    forvalues i = 1/`r(r)' {
        if freq[`i',1] > `max_freq' {
            local max_freq = freq[`i',1]
        }
    }
    local max_pct = `max_freq' / _N
    if `max_pct' > 0.85 {
        drop `var'
        display "Dropped `var' (max frequency: " %5.1f `max_pct'*100 "%)"
    }
}


