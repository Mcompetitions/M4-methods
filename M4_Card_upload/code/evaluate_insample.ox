#include <oxstd.oxh>
#import "ForecastAB"
#import "M4"

EvaluateForecasts(const sType, const bLimit, const cH=-1)
{
	decl m4 = new M4(sType, [=]() { return new ForecastAB();} );

	decl asnaive = {"naive2"};
	decl asmm = {"Delta", "Rho","Delta+C", "Rho+C","Card"};
	decl asmethods = asnaive ~ asmm;
	decl asstats = {"MAPE","sMAPE","MASE"};
//	decl asstats = {"MSIS","OUT","OUT1","OUT2","OUT3","OUT6","OUT12","OUT18","Above"};
	decl asrest = {"T"};

	decl aslabel = asstats, s, i;
	foreach (s in aslabel[i])
		aslabel[i] = s ~ "(%s)";
	aslabel = M4::Replicate(aslabel, asmethods);

	decl fn_run = [=](const oDB)
	{
		decl asy = oDB.GetAllNames(), ch = cH <= 0 ? m4.GetH() : cH;

		if (sizeof(asy) == 0)
			continue;
		
		oDB.Select("Y", asy);

		oDB.SetHoldBack(ch);
		if (bLimit)
			oDB.SetLimit(m4.GetLimit());
		oDB.AddForecasts(ch, asnaive, oDB.ForecastNaive);
		oDB.AddForecasts(ch, asmm,    oDB.Forecast);
		return {oDB.Collect(asstats, asmethods) | oDB.Collect(asrest), asy};
	};
	decl fn_get_forc = [=](const oDB, const sType)
	{
		return {oDB.CollectForecasts(sType), oDB.GetVarNames() };
	};

	m4.Apply(fn_run, aslabel ~ asrest);
	
	decl mres = m4.GetResults();
	decl mres_med = quantilec(mres');
	decl mres_mean = meanc(mres');

	decl cmethods = sizeof(asmethods);
	decl vstd = reshape(mres_mean, sizeof(asstats), cmethods)[][0];
	vstd = vecr(reshape(vstd, cmethods, sizeof(asstats))' )' ~ ones(1, sizeof(asrest));
	
	print("%r", {sType ~ "-median", sType ~ "-mean", sType ~ "-mean"}, "%c", aslabel ~ asrest ~ "K", "%13.3f", (mres_med | mres_mean | mres_mean ./ vstd) ~ sizec(mres));

	delete m4;
}

main()
{
	decl astypes = {"Yearly","Quarterly","Monthly","Weekly","Daily","Hourly"};
	decl astype1 = {"Yearly"};
	decl ism3 = 0;

	if (ism3)
	{
		M4::SetDataPath("../data_M3/");
		astypes = astypes[:2];
	}
	
	foreach (decl stype in astype1)
	{
		EvaluateForecasts(stype, TRUE);
	}
}
