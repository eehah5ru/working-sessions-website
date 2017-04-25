require "slim/include"

def field name
  return "$#{name.to_s}$"
end

def archive_image(file_name)
  return "/images/2016/archive/$canonical_name$/" + file_name
end
