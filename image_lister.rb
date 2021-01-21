def list(folder)
  string = Dir[folder].reduce "    [ " do |acc, fileName| acc + "\"#{fileName}\"\n    , " end
  string = string[0..-3] + "]"
end

content = "module Images exposing (..)\n\n"
content += "enteriorok =\n" + list("./assets/enteriorok/*") + "\n\n"
content += "latvanytervek =\n" + list("./assets/latvanytervek/*") + "\n\n"

IO.write("./src/Images.elm", content)
