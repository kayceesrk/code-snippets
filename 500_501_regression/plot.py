import pandas as pd
import seaborn as sb
import sys

df = pd.read_csv("data.csv")
df = df.rename(columns={"parameter_domains": "domains", "mean": "time"})
print(df)
plot = sb.lineplot(data=df,x="domains",y="time",hue="compiler")
plot.set_title(str(sys.argv[1]))
fig = plot.get_figure()
fig.savefig(sys.argv[1] + ".png")
