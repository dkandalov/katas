package xpday.questions

println Math.sqrt(100)
println Math.sqrt(100)

def question = "what is 0 multiplied by 19"
println question =~ /what is (\d+) multiplied by (\d+)/
def matcher = question =~ /what is (\d+) multiplied by (\d+)/
matcher.matches()
println matcher.group(0)
println matcher.group(1)
println matcher.group(2)