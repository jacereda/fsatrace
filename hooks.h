HOOK1(int
      , unlink
      , const char *
      , !r
      , 'd'
      , a0
      , 0)
HOOK2(FILE *
      , fopen
      , const char *
      , const char *
      , r
      , strchr(a1, 'r')? 'w' : 'r'
      , a0
      , 0)
HOOK2(int
      , rename
      , const char *
      , const char *
      , !r
      , 'm'
      , a1
      , a0)
HOOK3(int 
      , open
      , const char *
      , int
      , mode_t 
      , r >= 0
      , a1 & (O_RDWR | O_WRONLY | O_APPEND | O_CREAT | O_TRUNC) ? 'w' : 'r'
      , a0
      , 0)
