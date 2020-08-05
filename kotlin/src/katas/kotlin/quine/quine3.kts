// https://github.com/MakeNowJust/quine/blob/master/quine.kts

val s = """val s = ""%c%s%c""
print(s.format('"',s,'"'))
"""
print(s.format('"',s,'"'))