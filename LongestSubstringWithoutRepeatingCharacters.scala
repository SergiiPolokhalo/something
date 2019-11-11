//Longest Substring Without Repeating Characters

object Solution {
    def lengthOfLongestSubstring(s: String): Int = {
      s.length match {
        case 0 => 0
        case 1 => 1
        case l => {
          var max = 0
          var i=0
          var j=1
          //TODO
          for (j <- 1 until l) {
            var prev = s.substring(i,j)
            val next = s.substring(j,j+1)
            if (prev.contains(next)) {
              val delta = prev.lastIndexOf(next)
              i = i + delta
              prev = s.substring(i,j)
            }
            max = if (prev.size>max) prev.size else max
          }
          max
        }
      }
    }
}

var r = (
  Solution.lengthOfLongestSubstring("abcabcbb"),//3
  Solution.lengthOfLongestSubstring("bbbbb"),//1
  Solution.lengthOfLongestSubstring("pwwkew")//3
  )

println(r)
