class CLIPSEnvironmentWithUdf < CLIPS::Environment
  def add(a, b=2)
    a + b
  end

  def subtract(a, b=2)
    a - b
  end
end
