function confInt = Get95PercConfidenceInterval(std, numSamples)
z_95 = 1.96;
confInt = z_95 .* std ./ sqrt(numSamples);
end