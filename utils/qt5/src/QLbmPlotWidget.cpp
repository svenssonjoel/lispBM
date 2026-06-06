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

#include "QLbmPlotWidget.h"

#include <QVBoxLayout>

static const QList<QColor> s_palette = {
  QColor(0x1f, 0x77, 0xb4),
  QColor(0xff, 0x7f, 0x0e),
  QColor(0x2c, 0xa0, 0x2c),
  QColor(0xd6, 0x27, 0x28),
  QColor(0x94, 0x67, 0xbd),
  QColor(0x8c, 0x56, 0x4b),
  QColor(0xe3, 0x77, 0xc2),
  QColor(0x7f, 0x7f, 0x7f),
};

QLbmPlotWidget::QLbmPlotWidget(QWidget *parent)
  : QLbmWidget(parent)
  , m_plot(new QCustomPlot(this)) {
  QVBoxLayout *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  layout->addWidget(m_plot);
  setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  m_plot->legend->setVisible(true);
  m_plot->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom);
}

bool QLbmPlotWidget::validGraph(int graphId) const {
  return graphId >= 0 && graphId < m_plot->graphCount();
}

int QLbmPlotWidget::addGraph(const QString &name) {
  QCPGraph *g = m_plot->addGraph();
  int id = m_plot->graphCount() - 1;
  g->setName(name);
  g->setPen(QPen(s_palette[id % s_palette.size()], 1.5));
  m_maxPoints.append(-1);
  return id;
}

void QLbmPlotWidget::setData(int graphId, const QVector<double> &xs, const QVector<double> &ys) {
  if (!validGraph(graphId)) return;
  m_plot->graph(graphId)->setData(xs, ys, true);
}

void QLbmPlotWidget::addPoint(int graphId, double x, double y) {
  if (!validGraph(graphId)) return;
  QCPGraph *g = m_plot->graph(graphId);
  g->addData(x, y);
  int maxPts = m_maxPoints[graphId];
  if (maxPts > 0 && g->data()->size() > maxPts) {
    auto it = g->data()->constBegin() + (g->data()->size() - maxPts);
    g->data()->removeBefore(it->key);
  }
}

void QLbmPlotWidget::clearGraph(int graphId) {
  if (!validGraph(graphId)) return;
  m_plot->graph(graphId)->data()->clear();
}

void QLbmPlotWidget::clearAll() {
  for (int i = 0; i < m_plot->graphCount(); i++)
    m_plot->graph(i)->data()->clear();
}

void QLbmPlotWidget::rescale() {
  m_plot->rescaleAxes();
}

void QLbmPlotWidget::replot() {
  m_plot->replot();
}

void QLbmPlotWidget::setXLabel(const QString &label) {
  m_plot->xAxis->setLabel(label);
}

void QLbmPlotWidget::setYLabel(const QString &label) {
  m_plot->yAxis->setLabel(label);
}

void QLbmPlotWidget::setMaxPoints(int graphId, int maxPoints) {
  if (!validGraph(graphId)) return;
  m_maxPoints[graphId] = maxPoints;
}

void QLbmPlotWidget::setXLog(bool logarithmic) {
  if (logarithmic) {
    m_plot->xAxis->setScaleType(QCPAxis::stLogarithmic);
    m_plot->xAxis->setTicker(QSharedPointer<QCPAxisTickerLog>(new QCPAxisTickerLog));
  } else {
    m_plot->xAxis->setScaleType(QCPAxis::stLinear);
    m_plot->xAxis->setTicker(QSharedPointer<QCPAxisTicker>(new QCPAxisTicker));
  }
}

void QLbmPlotWidget::setYLog(bool logarithmic) {
  if (logarithmic) {
    m_plot->yAxis->setScaleType(QCPAxis::stLogarithmic);
    m_plot->yAxis->setTicker(QSharedPointer<QCPAxisTickerLog>(new QCPAxisTickerLog));
  } else {
    m_plot->yAxis->setScaleType(QCPAxis::stLinear);
    m_plot->yAxis->setTicker(QSharedPointer<QCPAxisTicker>(new QCPAxisTicker));
  }
}
