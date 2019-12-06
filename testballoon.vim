let s:counter = 0
let s:timer = -1

function! MyBalloonExpr()
  let s:counter += 1
  call timer_stop( s:timer )
  let s:message =
          \ 'Cursor is at line ' . v:beval_lnum .
          \', column ' . v:beval_col .
          \ ' of file ' .  bufname(v:beval_bufnr) .
          \ ' on word "' . v:beval_text . '"'
  let s:timer = timer_start( 0, 'RealBalloonExpr' )
endfunction

function! RealBalloonExpr(timer)
  echom 'MyBalloonExpr: ' . s:counter
  call balloon_show( s:message )
endfunction

set mouse=a
set ttymouse=sgr
set balloonexpr=MyBalloonExpr()
set balloondelay=250
set ballooneval
set balloonevalterm
