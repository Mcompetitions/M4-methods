#ifndef M4_INCLUDED
#define M4_INCLUDED

class M4
{
	M4(const sType, const fnNewDb);
	static SetDataPath(const sPath);
	static GetDataPath();
	static SortIndex(const asY);
	LoadResults(const sType, const sSubType);
	~M4();
	static AnnualizeDb(const db, const iAverage=1, const cTmin=10);
	static TransformDb(const db, const sMethod, const cTmin=0);
	Aggregate(const iAverage=1);
	Transform(const sTransform);
	Apply(const fnRun, const asR);
	CollectData(const fnRun, args);
	GetResults();
	GetNames();
	GetRowLabels();
	GetH();
	GetLimit();
	Save(const sType, const bSort=TRUE);
	static Replicate(const sLabel, const aArgs);
	ApplyVar(fnRun);
	GetVar(const sVar);
	GetFrequency();
	static Plot(iPlot, vCrit, sTitle, cBlock=0, dMax=.NaN);

	decl m_aoDb;	 ///< array of databases
	decl m_sType;	 ///< base name of files
	decl m_mResult;	 ///< matrix[cR][cY] results from apply
	decl m_asY;		 ///< array[asY] of variable names
	decl m_asR;		 ///< array[cR] of row labels

	static decl sm_sPath = "../data/"; ///< path to load data from
}
#endif // M4_INCLUDED
