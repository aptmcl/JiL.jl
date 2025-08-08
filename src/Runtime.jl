
export with, ThreadLocalParameter

function with(f, p, newvalue)
  oldvalue = p()
  p(newvalue)
  try
    f()
  finally
    p(oldvalue)
  end
end

with(f, p, newvalue, others...) =
  with(p, newvalue) do
    with(f, others...)
  end

struct ThreadLocalParameter{T}
  value::T
end

(p::ThreadLocalParameter{T})() where T = get(task_local_storage(), p, p.value)::T
(p::ThreadLocalParameter{T})(newvalue::T) where T = task_local_storage(p, newvalue)::T
