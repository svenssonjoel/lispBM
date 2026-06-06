/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef QLBMPLOTWIDGET_H_
#define QLBMPLOTWIDGET_H_

#include "QLbmWidget.h"

#include "qcustomplot.h"
#include <QVector>

class QLbmPlotWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmPlotWidget(QWidget *parent = nullptr);

  int  addGraph(const QString &name);
  void setData(int graphId, const QVector<double> &xs, const QVector<double> &ys);
  void addPoint(int graphId, double x, double y);
  void clearGraph(int graphId);
  void clearAll();
  void rescale();
  void replot();
  void setXLabel(const QString &label);
  void setYLabel(const QString &label);
  void setMaxPoints(int graphId, int maxPoints);  // -1 = unlimited
  void setXLog(bool logarithmic);
  void setYLog(bool logarithmic);

private:
  bool validGraph(int graphId) const;

  QCustomPlot *m_plot;
  QVector<int> m_maxPoints;
};

#endif // QLBMPLOTWIDGET_H_
